(ns xcl.external-js-content
  (:require [xcl.external :as ext]
            [xcl.content-interop :as ci
             :refer [get-all-text]]))

(def $WEB-CONTENT-ROOT "/")

(reset! ci/Node-TEXT_NODE (aget js/Node "TEXT_NODE"))

;; pdf
(when-let [pdfjsLib (aget js/window "pdfjsLib")]
  (ext/register-loader!
   "pdf"
   (fn [resource-address callback]
     (let [file-name (:resource-resolver-path resource-address)]
       (if-not file-name
         (js/alert (str "NO SUCH FILE: " file-name))
         (let [maybe-page-number-bound
               (some->> resource-address
                        (:content-resolvers)
                        (filter (fn [resolver]
                                  (= (:type resolver)
                                     :page-number)))
                        (first)
                        (:bound))]
           (let [rel-uri (str $WEB-CONTENT-ROOT
                              file-name)]
             (-> pdfjsLib
                 (.getDocument rel-uri)
                 (.then
                  (fn [pdf]
                    (let [count-promises (clj->js [])
                          page-beg (or (:beg maybe-page-number-bound) 1)
                          page-end (or (:end maybe-page-number-bound)
                                       (aget pdf "numPages"))]
                      (doseq [n (range page-beg (inc page-end))]
                        (let [page (.getPage pdf n)]
                          (.push count-promises
                                 (-> page
                                     (.then (fn [p]
                                              (-> p
                                                  (.getTextContent)
                                                  (.then (fn [text]
                                                           (-> (aget text "items")
                                                               (.map (fn [s]
                                                                       (aget s "str")))
                                                               (.join " ")))))))))))
                      (-> js/Promise
                          (.all count-promises)
                          (.then (fn [texts]
                                   (callback (.join texts " "))))))))))))))))

;; epub
(when-let [ePub (aget js/window "ePub")]
  (ext/register-loader!
   "epub"
   (fn [resource-address callback]
     (let [search-path (:resource-resolver-path resource-address)
           maybe-page-number-bound (some->> resource-address
                                            (:content-resolvers)
                                            (filter (fn [resolver]
                                                      (= (:type resolver)
                                                         :page-number)))
                                            (first)
                                            (:bound))
           rel-uri (str $WEB-CONTENT-ROOT
                        search-path)
           book (ePub rel-uri)]
       (-> (aget book "ready")
           (.then
            (fn []
              (let [dom-el (js/document.createElement "div")
                    num-pages (aget book "spine" "length")
                    page-beg (or (:beg maybe-page-number-bound)
                                 1)
                    page-end (or (:end maybe-page-number-bound)
                                 num-pages)
                    text-buffer (atom [])
                    ]
                (aset dom-el "style" "display:none;")
                (js/document.body.appendChild dom-el)
                (let [rendition (.renderTo
                                 book dom-el
                                 (clj->js {}))
                      load-pages!
                      (fn load-pages! [remain]
                        (if (empty? remain)
                          (do
                            (.clear rendition)
                            (-> dom-el
                                (aget "parentNode")
                                (.removeChild dom-el))
                            (->> @text-buffer
                                 (interpose "\n")
                                 (apply str)
                                 (callback)))
                          (let [page-index (first remain)]
                            (-> rendition
                                (.display page-index)
                                (.then
                                 (fn [page]
                                   (let [text-content
                                         (->> (get-all-text (aget page "document" "body"))
                                              (interpose "\n")
                                              (apply str))]
                                     (swap! text-buffer conj text-content))
                                   (load-pages! (rest remain))))))))]
                  (load-pages! (range (dec page-beg) page-end)))))))))))

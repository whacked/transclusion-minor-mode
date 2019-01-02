(ns xcl.external-js-content
  (:require [xcl.external :as ext]
            [xcl.content-interop :as ci
             :refer [get-all-text]]
            [xcl.pdfjslib-interop
             :refer [set-pdfjslib!
                     pdfjslib-load-text]]))

(def $WEB-CONTENT-ROOT "/")

(reset! ci/Node-TEXT_NODE (aget js/Node "TEXT_NODE"))

;; pdf
(when-let [pdfjsLib (aget js/window "pdfjsLib")]
  (set-pdfjslib! pdfjsLib)
  (ext/register-loader!
   "pdf"
   (fn [resource-address callback]
     (let [file-name (:resource-resolver-path resource-address)]
       (if-not file-name
         (js/alert (str "NO SUCH FILE: " file-name))
         (let [maybe-page-number-bound
               (ci/get-maybe-page-number-bound resource-address)]
           (let [rel-uri (str $WEB-CONTENT-ROOT
                              file-name)]
             (pdfjslib-load-text
              rel-uri
              (:beg maybe-page-number-bound)
              (:end maybe-page-number-bound)
              callback))))))))

;; epub
(when-let [ePub (aget js/window "ePub")]
  (ext/register-loader!
   "epub"
   (fn [resource-address callback]
     (let [search-path (:resource-resolver-path resource-address)
           maybe-page-number-bound (ci/get-maybe-page-number-bound
                                    resource-address)
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
                (aset dom-el "style"
                      (->> {:opacity 0.01
                            :position "absolute"
                            :left "-9999px"
                            :top "-9999px"
                            :width "999px"
                            :height "999px"}
                           (map (fn [[k v]]
                                  (str (name k) ":" v ";")))
                           (apply str)))
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
                                   (load-pages! (rest remain))))
                                (.catch
                                 (fn [err]
                                   (js/console.error err)))))))]
                  (load-pages! (range (dec page-beg) page-end)))))))))))

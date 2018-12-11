(ns xcl.node-interop
  (:require [xcl.core :as sc]
            [xcl.core :refer [render-transclusion]]
            [xcl.external :as ext]
            [xcl.pdfjslib-interop
             :refer [pdfjslib-load-text
                     set-pdfjslib!]]
            [xcl.content-interop :as ci
             :refer [get-all-text]]
            ["pdfjs-dist" :as pdfjsLib]
            ["epub" :as EPub]
            ["jsdom" :as jsdom]))

(def JSDOM (aget jsdom "JSDOM"))

(reset! ci/Node-TEXT_NODE
        (aget (new JSDOM) "window" "Node" "TEXT_NODE"))

(set-pdfjslib! pdfjsLib)
(ext/register-loader!
 "pdf"
 (fn [resource-address callback]
   (let [file-name (:resource-resolver-path resource-address)]
     (if-not file-name
       (js/console.warn (str "NO SUCH FILE: " file-name))
       (let [maybe-page-number-bound
             (ci/get-maybe-page-number-bound resource-address)]
         (let [rel-uri (str file-name)]
           (pdfjslib-load-text
            rel-uri
            (:beg maybe-page-number-bound)
            (:end maybe-page-number-bound)
            callback)))))))

(ext/register-loader!
 "epub"
 (fn [resource-address callback]
   (let [file-name (:resource-resolver-path resource-address)]
     (if-not file-name
       (js/console.warn (str "NO SUCH FILE: " file-name))
       (let [maybe-page-number-bound
             (ci/get-maybe-page-number-bound resource-address)
             epub (new EPub file-name)]
         (doto epub
           (.on "end"
                (fn []
                  (let [page-beg (or (:beg maybe-page-number-bound)
                                     1)
                        page-end (or (:end maybe-page-number-bound)
                                     (aget epub "flow" "length"))
                        page-keep (set (range (dec page-beg) page-end))
                        out (atom [])]
                    (->> (aget epub "flow")
                         (array-seq)
                         (map-indexed vector)
                         (filter (fn [[index _]]
                                   (page-keep index)))
                         (mapv (fn [[index chapter]]
                                 (.getChapter
                                  epub
                                  (aget chapter "id")
                                  (fn [err xhtml-string]
                                    (when err
                                      (js/console.error err))
                                    (swap!
                                     out
                                     conj
                                     [index
                                      (let [dom (new JSDOM xhtml-string)]
                                        ;; grab all text <p> elements
                                        (comment
                                          (->> (js-invoke
                                                (aget dom "window" "document")
                                                "querySelectorAll" "p")
                                               (array-seq)
                                               (map (fn [node]
                                                      (aget node "textContent")))
                                               (interpose " ")
                                               (apply str)))
                                        (->> (aget dom
                                                   "window" "document" "body")
                                             (get-all-text)
                                             (interpose "\n")
                                             (apply str)))])
                                    
                                    (when (= (count @out)
                                             (count page-keep))
                                      (->> @out
                                           (sort)
                                           (map last)
                                           (interpose " ")
                                           (apply str)
                                           (callback)))))))))))
           (.parse)))))))

(defn render
  "compatibility function for calling from nodejs; wraps all fns in
  postprocessor-coll to ensure resultant object is native #js type
  "
  [candidate-seq-loader content-loader source-text & postprocessor-coll]
  (->> postprocessor-coll
       (map (fn [postprocessor-fn]
              (fn [content xcl-spec depth]
                (postprocessor-fn content (clj->js xcl-spec) depth))))
       (apply render-transclusion
              candidate-seq-loader content-loader source-text)))

(ns xcl.node-interop
  (:require [xcl.core :as sc]
            [xcl.content-interop :as ci]
            [xcl.core :refer [render-transclusion]]
            [xcl.external :as ext]
            [xcl.pdfjslib-interop
             :refer [pdfjslib-load-text
                     set-pdfjslib!]]
            ["pdfjs-dist" :as pdfjsLib]
            [xcl.node-epub-interop :as epubi]))

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
             (ci/get-maybe-page-number-bound resource-address)]
         (epubi/load-and-get-text
          file-name
          (:beg maybe-page-number-bound)
          (:end maybe-page-number-bound)
          nil
          (fn [chapters]
            (->> chapters
                 (map :text)
                 (interpose " ")
                 (apply str)
                 (callback)))))))))

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

(ns xcl.node-interop
  (:require [xcl.core :refer [render-transclusion]]))

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

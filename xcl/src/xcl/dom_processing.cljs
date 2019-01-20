(ns xcl.dom-processing
  (:require ["jsdom" :as jsdom]
            [xcl.content-interop :as ci
             :refer [get-all-text]]))

(def JSDOM (aget jsdom "JSDOM"))
(reset! ci/Node-TEXT_NODE
        (aget (new JSDOM) "window" "Node" "TEXT_NODE"))

(defn xhtml-string->text
  [xhtml-string]
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
         (apply str))))

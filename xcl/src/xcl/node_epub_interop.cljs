(ns xcl.node-epub-interop
  (:require [xcl.content-interop :as ci
             :refer [get-all-text]]
            ["epub" :as EPub]
            ["jsdom" :as jsdom]))

(def JSDOM (aget jsdom "JSDOM"))

(reset! ci/Node-TEXT_NODE
        (aget (new JSDOM) "window" "Node" "TEXT_NODE"))

(defn load-and-get-text [file-name
                         maybe-page-beg
                         maybe-page-end
                         on-get-page
                         on-complete]
  (let [epub (new EPub file-name)]
    (doto epub
      (.on "end"
           (fn []
             (let [page-beg (or maybe-page-beg
                                1)
                   page-end (or maybe-page-end
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
                               
                               (let [dom (new JSDOM xhtml-string)
                                     textContent (->> (aget dom
                                                            "window" "document" "body")
                                                      (get-all-text)
                                                      (interpose "\n")
                                                      (apply str))
                                     section-data-out
                                     {:section index
                                      :chapter (aget chapter "id")
                                      :text textContent}]
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
                                 (when on-get-page
                                   (on-get-page section-data-out))
                                 
                                 (swap! out conj section-data-out))
                               
                               ;; TODO: may be more useful if preserve the section information.
                               ;; Ref node_interop.cljs:render() example run
                               (when (and
                                      on-complete
                                      (= (count @out)
                                         (count page-keep)))
                                 (->> @out
                                      (sort-by :section)
                                      (on-complete)))))))))))
      (.parse))))

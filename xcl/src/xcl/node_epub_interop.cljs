(ns xcl.node-epub-interop
  (:require [xcl.dom-processing :as domp]
            ["epub" :as EPub]))

(defn load-and-get-text [file-name
                         maybe-page-beg
                         maybe-page-end
                         on-get-section
                         on-complete-all-sections]
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
                               
                               (let [textContent (domp/xhtml-string->text
                                                  xhtml-string)
                                     section-data-out
                                     {:section index
                                      :chapter (aget chapter "id")
                                      :text textContent}]
                                 
                                 (when on-get-section
                                   (on-get-section section-data-out))
                                 
                                 (swap! out conj section-data-out))
                               
                               ;; TODO: may be more useful if preserve the section information.
                               ;; Ref node_interop.cljs:render() example run
                               (when (and
                                      on-complete-all-sections
                                      (= (count @out)
                                         (count page-keep)))
                                 (->> @out
                                      (sort-by :section)
                                      (on-complete-all-sections)))))))))))
      (.parse))))

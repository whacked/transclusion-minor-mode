(ns xcl.pdfjslib-interop)

(def pdfjsLib (atom nil))

(defn set-pdfjslib! [lib-object]
  (reset! pdfjsLib lib-object))

(defn pdfjslib-load-text
  [rel-uri
   maybe-page-beg
   maybe-page-end
   callback]
  ;; hack: to accommodate both browser and node loading, when using
  ;; set-pdfjslib!, we need to prevent Closure's symbol optimization.
  ;; How do we know this?
  ;; If you use instead, (-> (.getDocument @pdfjsLib) (.then ...)),
  ;; the library will work with optimizations turned off, but will
  ;; fail with errors like "Tb(...).ed is not a function" turned on.
  (let [getDocument (aget @pdfjsLib "getDocument")]
    (js-invoke
     (getDocument rel-uri)
     "then"
     (fn [pdf]
       (let [count-promises (clj->js [])
             page-beg (or maybe-page-beg 1)
             page-end (or maybe-page-end
                          (aget pdf "numPages"))]
         (doseq [n (range page-beg (inc page-end))]
           (let [page (js-invoke pdf "getPage" n)]
             (.push count-promises
                    (js-invoke
                     page
                     "then"
                     (fn [p]
                       (-> p
                           (js-invoke "getTextContent")
                           (js-invoke "then"
                                      (fn [text]
                                        (-> (aget text "items")
                                            (.map (fn [s]
                                                    (aget s "str")))
                                            (.join " "))))))))))
         (-> js/Promise
             (.all count-promises)
             (.then (fn [texts]
                      (callback (.join texts " "))))))))))


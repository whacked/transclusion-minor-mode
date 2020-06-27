(ns xcl.pdfjslib-interop
  (:require ["fs" :as fs]))

(def pdfjsLib (atom nil))

(defn set-pdfjslib! [lib-object]
  (reset! pdfjsLib lib-object))

(defn pdf-page-text-items-to-string [pdf-text]
  (-> (aget pdf-text "items")
      (.map (fn [s]
              (-> (aget s "str")
                  (.replace "@" "@@"))))
      (.join "@n")
      (clojure.string/replace #"-@n" "-")
      (clojure.string/replace #"@n" "\n")
      (clojure.string/replace #"@@" "@")))

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
     (aget (getDocument rel-uri) "promise")
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
                                      pdf-page-text-items-to-string)))))))
         (-> js/Promise
             (.all count-promises)
             (.then (fn [texts]
                      (callback (.join texts " "))))))))))

(defn process-pdf
  ([file-path on-get-page-text on-complete-page-texts]
   (process-pdf
    file-path on-get-page-text on-complete-page-texts
    (fn [err]
      (js/console.error err))))
  ([file-path
    on-get-page-text       ;; [page-num text] -> nil
    on-complete-page-texts ;; [page-texts] -> nil
    on-error]
   (if-not (.existsSync fs file-path)
     (js/console.error
      (str "ERROR: file [" file-path "] does not exist"))
     (let [rel-uri (str "file:///" file-path)
           getDocument (aget @pdfjsLib "getDocument")]
       (js-invoke
        (aget (getDocument rel-uri) "promise")
        "then"
        (fn [pdf]
          (println "GOT PDF!" rel-uri)
          (let [count-promises (clj->js [])
                page-beg 1
                page-end (aget pdf "numPages")]
            (doseq [page-num (range page-beg (inc page-end))]
              (let [page (js-invoke pdf "getPage" page-num)]
                (.push count-promises
                       (js-invoke
                        page
                        "then"
                        (fn [p]
                          (-> p
                              (js-invoke "getTextContent")
                              (js-invoke "then"
                                         (fn [pdf-page-text]
                                           (on-get-page-text
                                            page-num
                                            (pdf-page-text-items-to-string
                                             pdf-page-text))))))))))
            (-> js/Promise
                (.all count-promises)
                (.then on-complete-page-texts)
                (.catch on-error)))))))))

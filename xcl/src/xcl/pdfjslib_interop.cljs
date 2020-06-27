(ns xcl.pdfjslib-interop
  (:require ["fs" :as fs]))

(def pdfjsLib (atom nil))

(defn set-pdfjslib! [lib-object]
  (reset! pdfjsLib lib-object))

(defn pdf-page-text-items-to-string [pdf-text]
  (loop [items (array-seq (aget pdf-text "items"))
         last-x-end -1
         last-y-beg -1
         average-char-width 0
         out []]
    (if (empty? items)
      (apply str out)
      (let [item (first items)
            bbox-width (aget item "width")
            xfm-matrix (aget item "transform")
            item-translate-x (aget xfm-matrix 4)
            item-translate-y (aget xfm-matrix 5)
            x-offset-from-previous (- item-translate-x
                                      last-x-end)
            item-text (aget item "str")
            cur-item-count (count out)]

        (recur (rest items)
               (+ bbox-width item-translate-x)
               item-translate-y
               (/ bbox-width (count item-text))
               (if (= item-translate-y last-y-beg)
                   ;; same line
                 
                   (cond (= cur-item-count 0)
                         ;; first item
                         (conj out item-text)

                         (< x-offset-from-previous
                            (/ average-char-width 4))
                         ;; assume same word
                         (update out (dec cur-item-count) str item-text)

                         :else
                         (conj out " " item-text))
                   ;; (conj out item-text)

                   ;; new line
                   (conj out "\n" item-text)))))))

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

(ns xcl.common)

(defn conj-if-non-nil [coll & maybe-nils]
  (->> maybe-nils
       (remove nil?)
       (apply conj coll)))

(defn re-pos
  ;; https://stackoverflow.com/a/18737013
  [re s]
  (let [re (js/RegExp. (.-source re) "g")]
    (loop [res {}]
      (if-let [m (.exec re s)]
        (recur (assoc res (.-index m) m))
        res))))

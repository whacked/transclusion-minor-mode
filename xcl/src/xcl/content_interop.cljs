(ns xcl.content-interop
  )

(defn log [& ss]
  (apply js/console.log ss))

(defn warn [& ss]
  (apply js/console.warn ss))

(defn find-first-matching-string-element [spec string-elements]
  (let [match-tokens
        (some-> (get-in spec [:content-boundary :substring])
                (clojure.string/lower-case)
                (clojure.string/split #"\+"))]
    (some->> string-elements
             (filter (fn [line]
                       (every?
                        (fn [token]
                          (-> line
                              (clojure.string/lower-case)
                              (clojure.string/index-of token)))
                        match-tokens)))
             (first))))

(def $resolver
  (atom {:whole-file (fn [_ content] content)
         :line-range (fn [spec content]
                       (let [{:keys [beg end]} (:content-boundary spec)
                             begin-index (max 0 (if beg (dec beg) 0))
                             lines (->> (clojure.string/split-lines content)
                                        (drop begin-index))]
                         (->> (if-not end
                                lines
                                (take (- end begin-index)
                                      lines))
                              (interpose "\n")
                              (apply str))))
         :char-range (fn [spec content]
                       (let [{:keys [beg end]} (:content-boundary spec)]
                         (subs content beg
                               (or end (count content)))))
         :percent-range (fn [spec content]
                          (let [lines (clojure.string/split-lines content)
                                n-lines (count lines)
                                {:keys [beg end]} (:content-boundary spec)
                                beg-index (->> (* 0.01 beg n-lines)
                                               (Math/round)
                                               (max 0)) 
                                end-index (->> (* 0.01 end n-lines)
                                               (Math/round))]
                            (->> lines
                                 (take (inc end-index))
                                 (drop beg-index)
                                 (interpose "\n")
                                 (apply str))))
         :token-bound (fn [spec content]
                        (let [{:keys [token-beg
                                      token-end]}
                              (:content-boundary spec)
                              lower-content (clojure.string/lower-case
                                             content)
                              maybe-begin-index (clojure.string/index-of
                                                 lower-content
                                                 (clojure.string/lower-case
                                                  token-beg))]
                          (when maybe-begin-index
                            (when-let [maybe-end-index
                                       (clojure.string/index-of
                                        lower-content
                                        (clojure.string/lower-case token-end)
                                        (+ maybe-begin-index
                                           (count token-beg)))]
                              (subs content
                                    maybe-begin-index
                                    (+ maybe-end-index (count token-end)))))))
         
         :line-with-match (fn [spec content]
                            (find-first-matching-string-element
                             spec (clojure.string/split-lines content)))
         
         :paragraph-with-match (fn [spec content]
                                 (find-first-matching-string-element
                                  spec (clojure.string/split content #"[\r\t ]*\n[\r\t ]*\n[\r\t ]*")))
         }))

(defn get-resolver [resolver-name]
  (if-let [registered-resolver (@$resolver resolver-name)]
    registered-resolver
    (do
      (warn "UNKNOWN RESOLVER" (pr-str resolver-name))
      (@$resolver :whole-file))))

(defn resolve-content [resolved-spec content]
  (let [resolver (-> (:content-resolver resolved-spec :whole-file)
                     (get-resolver))]
    (resolver resolved-spec content)))

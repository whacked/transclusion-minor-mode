(ns xcl.content-interop)

(defn log [& ss]
  (apply js/console.log ss))

(defn warn [& ss]
  (apply js/console.warn ss))

(defn find-first-matching-string-element [spec string-elements]
  (let [match-tokens
        (some-> (get-in spec [:content-boundary :query-string])
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

(defn re-pos
  ;; https://stackoverflow.com/a/18737013
  [re s]
  (let [re (js/RegExp. (.-source re) "g")]
    (loop [res {}]
      (if-let [m (.exec re s)]
        (recur (assoc res (.-index m)
                      (rest m)))
        res))))

(defn get-org-heading-positions [org-content]
  (let [fake-padded-content (str "\n" org-content)]
    (->> (re-pos #"\n(\*+)\s+([^\n]+)"
                 fake-padded-content)
         (into {}))))

(defn get-org-drawer-data [org-content]
  (let [drawer-pattern #"(\s*):([^:]+):\s*(\S*)\s*"
        lines (clojure.string/split-lines org-content)]
    (loop [remain lines
           current-drawer-name nil
           current-drawer-indent-level nil
           buffer {}
           out {}]
      (if (empty? remain)
        out
        (let [line (first remain)
              maybe-match (re-find drawer-pattern line)]
          (if-not maybe-match
            (if current-drawer-name
              (recur (rest remain)
                     current-drawer-name
                     current-drawer-indent-level
                     (update buffer :text
                             (fn [cur-text-coll]
                               (conj (or cur-text-coll [])
                                     (clojure.string/trim line))))
                     out)
              (recur (rest remain)
                     current-drawer-name
                     current-drawer-indent-level
                     buffer
                     out))
            (let [[indentation key-name maybe-value]
                  (rest maybe-match)]
              ;; (println ">> KEY" key-name ":::" maybe-value)
              (cond (nil? current-drawer-name)
                    ;; open a new drawer
                    (recur (rest remain)
                           (keyword key-name)
                           (count indentation)
                           {} out)

                    (and current-drawer-name (= key-name "END"))
                    ;; complete the drawer
                    (recur (rest remain)
                           nil
                           nil
                           {}
                           (assoc out current-drawer-name buffer))
                    
                    (and current-drawer-name
                         (= (count indentation)
                            current-drawer-indent-level))
                    ;; collect the attribute
                    (recur (rest remain)
                           current-drawer-name
                           current-drawer-indent-level
                           (assoc buffer (keyword key-name) maybe-value)
                           out)
                    
                    :else
                    (recur (rest remain)
                           current-drawer-name
                           current-drawer-indent-level
                           buffer
                           out)))))))))

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
         :org-heading (fn [spec content]
                        (let [target-heading (-> spec
                                                 (get-in [:content-boundary :heading])
                                                 (clojure.string/lower-case))
                              org-heading-positions (get-org-heading-positions
                                                     content)]
                          (when-not (empty? org-heading-positions)
                            (loop [remain (sort (keys org-heading-positions))
                                   match-level nil
                                   beg nil
                                   end nil]
                              (if (or (empty? remain)
                                      (and match-level beg end))
                                (subs content beg (or end (count content)))
                                (let [index (first remain)
                                      [stars heading-text]
                                      (org-heading-positions index)]
                                  (if beg
                                    (if (<= (count stars) match-level)
                                      (recur (rest remain)
                                             match-level
                                             beg index))
                                    (if (= target-heading heading-text)
                                      ;; after finding a match,
                                      ;; we need to continue to the section end
                                      (recur (rest remain)
                                             (count stars)
                                             index end)
                                      (recur (rest remain)
                                             match-level
                                             beg end)))))))))
         :org-section-with-match
         (fn [spec content]
           (let [match-pattern (-> spec
                                   (get-in [:content-boundary :query-string])
                                   (clojure.string/lower-case)
                                   (clojure.string/replace #"\+" "\\s+.*?")
                                   (re-pattern))
                 org-heading-positions (get-org-heading-positions
                                        content)]
             (let [maybe-match (re-pos match-pattern content)]
               (when-not (empty? maybe-match)
                 (let [match-index (first (keys maybe-match))
                       [pre-matches post-matches]
                       (->> (sort (keys org-heading-positions))
                            (map (fn [index]
                                   [index (org-heading-positions index)]))
                            (split-with
                             (fn [[index _]]
                               (< index match-index))))
                       [parent-index [stars heading-text]] (last pre-matches)
                       parent-level (count stars)]
                   (loop [remain post-matches
                          end-index nil]
                     (if (or (empty? remain)
                             end-index)
                       (subs content parent-index
                             (or end-index (count content)))
                       (recur (rest remain)
                              (let [[index [stars _]] (first remain)
                                    heading-level (count stars)]
                                (if (<= heading-level parent-level)
                                  index
                                  end-index))))))))))
         :org-node-id
         (fn [spec content]
           (let [target-custom-id (-> spec
                                      (get-in [:content-boundary :id])
                                      (clojure.string/lower-case))
                 org-heading-positions (get-org-heading-positions
                                        content)
                 heading-index-coll (map first org-heading-positions)
                 section-index-pair-coll (map vector
                                              heading-index-coll
                                              (concat (rest heading-index-coll)
                                                      [(count content)]))]
             (loop [remain section-index-pair-coll]
               (let [[section-beg section-end] (first remain)
                     section-text (subs content section-beg section-end)
                     drawer-data (get-org-drawer-data section-text)]
                 (when-not (empty? remain)
                   (if (= target-custom-id
                          (get-in drawer-data
                                  [:PROPERTIES :CUSTOM_ID]))
                     section-text
                     (recur (rest remain))))))))}))

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

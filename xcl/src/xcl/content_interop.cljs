(ns xcl.content-interop
  (:require [xcl.common :refer [re-pos]]
            [xcl.external :as ext]))

(defn log [& ss]
  (apply js/console.log ss))

(defn warn [& ss]
  (apply js/console.warn ss))

(defn find-first-matching-string-element [spec string-elements]
  (let [match-tokens
        (some-> (get-in spec [:bound :query-string])
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

(defn get-org-heading-positions [org-content]
  (let [fake-padded-content (str "\n" org-content)]
    (->> (re-pos #"\n(\*+)\s+([^\n]+)"
                 fake-padded-content)
         (map (fn [[idx match]]
                [idx (rest match)]))
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
                       (let [{:keys [beg end]} (:bound spec)
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
                       (let [{:keys [beg end]} (:bound spec)]
                         (subs content beg
                               (or end (count content)))))
         :percent-range (fn [spec content]
                          (let [lines (clojure.string/split-lines content)
                                n-lines (count lines)
                                {:keys [beg end]} (:bound spec)
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
                              (:bound spec)
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
                                                 (get-in [:bound :heading])
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
                                   (get-in [:bound :query-string])
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
                                      (get-in [:bound :id])
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

(defn get-resolver [resolver-spec]
  (if-let [registered-resolver (@$resolver
                                (:type resolver-spec))]
    registered-resolver
    (do
      (warn "UNKNOWN RESOLVER"
            (pr-str (:type resolver-spec))
            " in "
            (pr-str resolver-spec))
      (@$resolver :whole-file))))

(defn resolve-content [resolved-spec content]
  (when content
    (let [final-resolver-spec
          (->> resolved-spec
               (:content-resolvers)
               (last))
          
          resolver (get-resolver
                    final-resolver-spec)]
      (resolver final-resolver-spec content))))

;; attempt to load external content loaders
(def $IS-IN-BROWSER (and js/window))

(def $WEB-CONTENT-ROOT "/")

;; pdf
(when-let [pdfjsLib (aget js/window "pdfjsLib")]
  (ext/register-loader!
   "pdf"
   (fn [resource-address callback]
     (let [file-name (:resource-resolver-path resource-address)]
       (if-not file-name
         (js/alert (str "NO SUCH FILE: " file-name))
         (let [maybe-page-number-bound
               (some->> resource-address
                        (:content-resolvers)
                        (filter (fn [resolver]
                                  (= (:type resolver)
                                     :page-number)))
                        (first)
                        (:bound))]
           (let [rel-uri (str $WEB-CONTENT-ROOT
                              file-name)]
             (-> pdfjsLib
                 (.getDocument rel-uri)
                 (.then
                  (fn [pdf]
                    (let [count-promises (clj->js [])
                          page-beg (or (:beg maybe-page-number-bound) 1)
                          page-end (or (:end maybe-page-number-bound)
                                       (aget pdf "numPages"))]
                      (doseq [n (range page-beg (inc page-end))]
                        (let [page (.getPage pdf n)]
                          (.push count-promises
                                 (-> page
                                     (.then (fn [p]
                                              (-> p
                                                  (.getTextContent)
                                                  (.then (fn [text]
                                                           (-> (aget text "items")
                                                               (.map (fn [s]
                                                                       (aget s "str")))
                                                               (.join " ")))))))))))
                      (-> js/Promise
                          (.all count-promises)
                          (.then (fn [texts]
                                   (callback (.join texts " "))))))))))))))))

(def Node-TEXT_NODE (aget js/Node "TEXT_NODE"))
;; using .innerText or .textContent causes <br> tags to be collapsed,
;; and we end up with strange text fragments.  couldn't find an easy
;; way to concatenate all text nodes with extra spacing, so here we
;; use a custom text node collector.
(defn get-all-text
  ([dom-node]
   (get-all-text dom-node []))
  ([dom-node out]
   (if (= (aget dom-node "nodeType")
          Node-TEXT_NODE)
     (conj out (clojure.string/trim
                (aget dom-node "textContent")))
     (->> (loop [remain (some->> (aget dom-node "childNodes")
                                 (array-seq))
                 collected []]
            (if (empty? remain)
              collected
              (recur (rest remain)
                     (concat collected
                             (get-all-text (first remain) out)))))
          (concat out)
          (vec)))))

;; epub
(when-let [ePub (aget js/window "ePub")]
  (ext/register-loader!
   "epub"
   (fn [resource-address callback]
     (let [search-path (:resource-resolver-path resource-address)
           maybe-page-number-bound (some->> resource-address
                                            (:content-resolvers)
                                            (filter (fn [resolver]
                                                      (= (:type resolver)
                                                         :page-number)))
                                            (first)
                                            (:bound))
           rel-uri (str $WEB-CONTENT-ROOT
                        search-path)
           book (ePub rel-uri)]
       (-> (aget book "ready")
           (.then
            (fn []
              (let [dom-el (js/document.createElement "div")
                    num-pages (aget book "spine" "length")
                    page-beg (or (:beg maybe-page-number-bound)
                                 1)
                    page-end (or (:end maybe-page-number-bound)
                                 num-pages)
                    text-buffer (atom [])
                    ]
                (aset dom-el "style" "display:none;")
                (js/document.body.appendChild dom-el)
                (let [rendition (.renderTo
                                 book dom-el
                                 (clj->js {}))
                      load-pages!
                      (fn load-pages! [remain]
                        (if (empty? remain)
                          (do
                            (.clear rendition)
                            (-> dom-el
                                (aget "parentNode")
                                (.removeChild dom-el))
                            (->> @text-buffer
                                 (interpose "\n")
                                 (apply str)
                                 (callback)))
                          (let [page-index (first remain)]
                            (-> rendition
                                (.display page-index)
                                (.then
                                 (fn [page]
                                   (let [text-content
                                         (->> (get-all-text (aget page "document" "body"))
                                              (interpose "\n")
                                              (apply str))]
                                     (swap! text-buffer conj text-content))
                                   (load-pages! (rest remain))))))))]
                  (load-pages! (range (dec page-beg) page-end)))))))))))

(ns xcl.core
  (:require [cemerick.url :refer (url url-encode)]
            [xcl.content-interop :as ci]
            [xcl.common :refer [re-pos conj-if-non-nil
                                get-file-extension]]))

;; TODO
;; [ ] anchor text deriver
;; [ ] anchor text resolver

(def known-protocols
  #{:file
    :grep
    :xcl})

(def protocol-matcher
  (-> (str
       "^\\s*(?:("
       (->> known-protocols
            (map name)
            (interpose "|")
            (apply str))
       "):)?(.+)\\s*$")
      (re-pattern)))

(def link-matcher
  (-> (str
       "([^:?]+)"
       "(::|[?])?((.+))?")
      (re-pattern)))

(defn make-named-matcher [pattern keys & [types]]
  (fn [s]
    (let [matches (rest (re-find pattern s))]
      (if (some identity matches)
        (zipmap keys
                (if-not types
                  matches
                  (map (fn [converter s]
                         (converter s))
                       types matches)))))))

(defn string-to-int [s]
  (when s (js/parseInt s)))

(defn string-to-float [s]
  (when s (js/parseFloat s)))

(defn web-query-to-string [s]
  (some-> s
          (clojure.string/replace #"%20" " ")))

(defmulti get-resource-match
  (fn [_ _ resolved]
    (:resource-resolver-method resolved)))

(defn get-file-exact
  [candidate-seq-loader content-loader resolved]
  (let [file-name (:path resolved)]
    (some->> (candidate-seq-loader)
             (filter (partial = file-name))
             (first))))

(defmethod get-resource-match :exact-name
  [candidate-seq-loader content-loader resolved]
  (get-file-exact candidate-seq-loader content-loader resolved))

(defmethod get-resource-match :exact-name-with-subsection
  [candidate-seq-loader content-loader resolved]
  (get-file-exact candidate-seq-loader content-loader resolved))

(defmethod get-resource-match :glob-name
  [candidate-seq-loader content-loader resolved]
  (let [file-pattern (-> (:path resolved)
                         (clojure.string/replace "*" ".*")
                         (re-pattern))]
    (some->> (candidate-seq-loader)
             (filter (partial re-find file-pattern))
             (first))))

(defmethod get-resource-match :grep-content
  [candidate-seq-loader content-loader resolved]
  (let [grep-pattern (-> (:path resolved)
                         (clojure.string/replace "+" " ")
                         (clojure.string/replace "%20" " ")
                         (re-pattern))]
    (some->> (candidate-seq-loader)
             (filter (fn [fname]
                       (some->> fname
                                (content-loader)
                                (re-find grep-pattern))))
             (first))))

(def org-style-range-matchers
  [[:line-range
    (let [line-range-matcher
          (make-named-matcher #"^(\d+)?(-)?(\d+)?$"
                              [:beg :range-marker :end]
                              [string-to-int
                               identity
                               string-to-int])]
      (fn [s]
        (when-let [match (line-range-matcher s)]
          (if-not (:range-marker match)
            (assoc (select-keys match [:beg])
                   :end (:beg match))
            (select-keys match [:beg :end])))))]
   [:char-range
    (make-named-matcher #"^(\d*)?,(\d*)?$"
                        [:beg :end]
                        [string-to-int string-to-int])]
   [:percent-range
    (let [percent-range-matcher
          (make-named-matcher #"^([.\d]+%)?(-)([.\d]+%)?$"
                              [:beg :range-marker :end]
                              [string-to-float
                               identity
                               string-to-float])]
      (fn [s]
        (when-let [match (percent-range-matcher s)]
          (if-not (:range-marker match)
            (assoc (select-keys match [:beg])
                   :end (:beg match))
            (select-keys match [:beg :end])))))]
   [:org-heading
    (make-named-matcher #"^\*\s*([\w ]+)$" [:heading])]
   [:token-bound
    (make-named-matcher #"^(\S.+)\.\.\.(\S.+)$" [:token-beg :token-end])]
   ])

(def url-style-constrictor-matchers
  [[:org-node-id
    (make-named-matcher #"id=(\S+)" [:id])]
   
   [:page-number
    (let [page-range-matcher
          (make-named-matcher #"p=(\d+)?(-)?(\d+)?"
                              [:beg :range-marker :end]
                              [string-to-int
                               identity
                               string-to-int])]
      (fn [s]
        (when-let [match (page-range-matcher s)]
          (if-not (:range-marker match)
            (assoc (select-keys match [:beg])
                   :end (:beg match))
            (select-keys match [:beg :end])))))]
   
   [:token-bound
    (make-named-matcher #"s=(\S.+)\.\.\.(\S.+)"
                        [:token-beg :token-end])]
   [:line-with-match
    (make-named-matcher #"line=(\S+)"
                        [:query-string]
                        [web-query-to-string])]
   [:paragraph-with-match
    (make-named-matcher #"para=(\S+)"
                        [:query-string]
                        [web-query-to-string])]
   [:org-section-with-match
    (make-named-matcher #"section=(\S+)"
                        [:query-string]
                        [web-query-to-string])]
   ])

(defn get-resource-resolver-method-for-file-by-type [file-path]
  (case (get-file-extension file-path)
    ("pdf" "epub") :exact-name-with-subsection
    :exact-name))

(defn parse-link [link]
  (let [[maybe-protocol maybe-remainder]
        (rest (re-find protocol-matcher link))
        
        protocol (keyword (or maybe-protocol "file"))
        remainder (or maybe-remainder link)
        
        [path maybe-qualifier-separator maybe-qualifier]
        (rest (re-find link-matcher remainder))
        
        maybe-resolvers
        (when-not (empty? maybe-qualifier)
          (loop [matcher-remain
                 (case maybe-qualifier-separator
                   "::" org-style-range-matchers
                   "?" url-style-constrictor-matchers
                   nil)
                 out []]
            (if (empty? matcher-remain)
              out
              (let [[range-type matcher]
                    (first matcher-remain)
                    maybe-match (matcher maybe-qualifier)]
                (recur
                 (rest matcher-remain)
                 (if maybe-match
                   (conj out
                         {:bound maybe-match
                          :type range-type})
                   out))))))
        ]
    
    {:link link
     :protocol protocol
     :resource-resolver-path (js/decodeURI path)
     :resource-resolver-method (cond (re-find #"\*" path)
                                     :glob-name

                                     (= protocol :grep)
                                     :grep-content
                                     
                                     :else :exact-name)
     :content-resolvers (or maybe-resolvers [{:type :whole-file}])}))

(defn get-resource-match-async
  [candidate-seq-loader-async
   content-loader-async
   link
   callback-on-received-match]

  (let [resolved (parse-link link)
        resource-resolver-path (:resource-resolver-path resolved)
        resource-resolver-method (:resource-resolver-method resolved)
        content-matcher-async
        (case resource-resolver-method
          :exact-name (fn [matching-resources]
                        (when-let [match (->> matching-resources
                                              (filter
                                               (partial = resource-resolver-path))
                                              (first))]
                          (callback-on-received-match
                           (assoc resolved
                                  :resource-resolver-path match))))

          :glob-name (fn [matching-resources]
                       (let [file-pattern (-> resource-resolver-path
                                              (clojure.string/replace "*" ".*")
                                              (re-pattern)) ]
                         (when-let [match (->> matching-resources
                                               (filter
                                                (fn [p] (re-find file-pattern p)))
                                               (first))]
                           (callback-on-received-match
                            (assoc resolved
                                   :resource-resolver-path match)))))

          :grep-content (fn [matching-resources]
                          (let [grep-pattern (-> resource-resolver-path
                                                 (clojure.string/replace "+" " ")
                                                 (clojure.string/replace "%20" " ")
                                                 (re-pattern))
                                maybe-load-content-async!
                                (fn maybe-load-content-async! [candidates]
                                  (when-not (empty? candidates)
                                    (let [candidate-spec (first candidates)
                                          fname (:resource-resolver-path candidate-spec)]
                                      (content-loader-async
                                       (assoc resolved
                                              :resource-resolver-path
                                              fname)
                                       (fn [content]
                                         (if (re-find grep-pattern content)
                                           (callback-on-received-match
                                            candidate-spec)
                                           (maybe-load-content-async!
                                            (rest candidates))))))))]
                            (->> matching-resources
                                 (map (fn [match-name]
                                        (assoc resolved
                                               :resource-resolver-path match-name)))
                                 (maybe-load-content-async!))))
          
          (fn [matching-resources]
            (js/console.error (str "ERROR: unhandled resolver: "
                                   resource-resolver-method))))]
    (candidate-seq-loader-async
     resource-resolver-path
     (fn [matching-resources]
       (content-matcher-async matching-resources)))))

    
    (js/console.log
     (str "%c===content resolver===%c"
          (pr-str content-resolver-method))
     "color:yellow;background:black;"
     "color:brown;background:none;")
    
    (let [base-spec {:link link
                     :path path
                     :protocol protocol
                     :resource-resolver-method (cond (re-find #"\*" path)
                                                     :glob-name

                                                     (= protocol :grep)
                                                     :grep-content
                                                  
                                                     :else (get-resource-resolver-method-for-file-by-type
                                                            path))
                     :content-resolver-method content-resolver-method
                     :content-boundary (:bound content-resolver-method)}

          resolved-spec (assoc base-spec
                               :content-resolver-method-type
                               (case (:resource-resolver-method base-spec)
                                 :exact-name-with-subsection
                                 (if (= :page-number
                                        (:type content-resolver-method))
                                   (get-in
                                    content-resolver-method
                                    [:sub :type]))
                                 
                                 (:type content-resolver-method)))
          file-name (get-resource-match
                     candidate-seq-loader
                     content-loader-async
                     resolved-spec)]
      (js/console.log
       (str "%cSPEC%c"
            (pr-str resolved-spec))
       "color:red;font-weight:bold;"
       "color:blue;")
      (assoc resolved-spec
             :resource-address (case (:resource-resolver-method resolved-spec)
                                 :exact-name-with-subsection
                                 (merge {:file-name file-name}
                                        (if (= :page-number
                                               (:type content-resolver-method))
                                          (:bound content-resolver-method)))
                                 
                                 {:file-name file-name})
             :match-content (some-> resolved-spec
                                    (ci/resolve-content
                                     (content-loader-async file-name))
                                    (clojure.string/trim))))))

(def transclusion-directive-matcher
  #"\{\{\{transclude\(([^\)]+)\)\}\}\}")

(defn parse-transclusion-directive [text]
  (re-pos transclusion-directive-matcher text))

(defn resolve-resource-address [resource-address]
  (if (string? resource-address)
    resource-address
    (:file-name resource-address)))

(defn render-transclusion
  "`candidate-seq-loader` should be a function which
   returns a seq of all the known file names
   `content-loader` should be a function which,
   when passed the :resource-address parameter from a `resolved-spec`,
   returns the full text of the target resource (generally a file)
   `postprocessor-coll` is potentially an iterable of functions
   of type (str, transclusion-spec, depth) -> str

  the postprocessing will be applied after every transclusion operation
  "
  [candidate-seq-loader content-loader
   source-text & postprocessor-coll]
  (let [inner-renderer
        (fn inner-renderer [depth
                            visited?
                            candidate-seq-loader
                            content-loader
                            source-text
                            & postprocessor-coll]
          (loop [remain (parse-transclusion-directive source-text)
                 prev-index 0
                 buffer []]
            (if (empty? remain)
              (->> (if (< prev-index (count source-text))
                     (subs source-text prev-index))
                   (conj buffer)
                   (apply str))
              (let [[match-index [matched-string
                                  matched-path]] (first remain)
                    interim-string (when (< prev-index match-index)
                                     (subs source-text prev-index match-index))
                    resolved-spec (parse-link matched-path)
                    resolved-file-name (resolve-resource-address
                                        (:resource-resolver-path resolved-spec))

                    postprocess
                    (fn [content]
                      (reduce (fn [input postprocessor]
                                (postprocessor
                                 input resolved-spec depth))
                              content
                              postprocessor-coll))

                    maybe-content (content-loader resolved-file-name)
                    ]
                (if (visited? resolved-file-name)
                  source-text
                  (recur (rest remain)
                         (+ match-index (count matched-string))
                         (conj-if-non-nil
                          buffer
                          interim-string
                          (if-let [rendered-string
                                   (when maybe-content
                                     (some-> (ci/resolve-content
                                              resolved-spec
                                              maybe-content)
                                             (clojure.string/trim)))]
                            (postprocess
                             (apply
                              inner-renderer
                              (inc depth)
                              (conj visited? resolved-file-name)
                              candidate-seq-loader
                              content-loader
                              rendered-string
                              postprocessor-coll))
                            matched-string))))))))]
    (apply inner-renderer
           1   ;; first detected transclusion starts at depth 1
           #{} ;; visited?
           candidate-seq-loader content-loader source-text
           postprocessor-coll)))

(defn render-transclusion-nodejs
  "compatibility function for calling from nodejs; wraps all fns in
  postprocessor-coll to ensure resultant object is native #js type
  "
  [candidate-seq-loader content-loader source-text & postprocessor-coll]
  (->> postprocessor-coll
       (map (fn [postprocessor-fn]
              (fn [content xcl-spec depth]
                (postprocessor-fn content (clj->js xcl-spec) depth))))
       (apply render-transclusion
              candidate-seq-loader content-loader source-text)))

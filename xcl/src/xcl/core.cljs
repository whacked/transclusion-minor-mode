(ns xcl.core
  (:require [cemerick.url :refer (url url-encode)]
            [xcl.content-interop :as ci]
            [xcl.common :refer [re-pos conj-if-non-nil]]
            ;; sample corpus
            [xcl.corpus :as corpus]
            ))

;; TODO
;; [ ] anchor text deriver
;; [ ] anchor text resolver

(js/console.clear)

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
   [:token-bound
    (make-named-matcher #"s=(\S.+)\.\.\.(\S.+)" [:token-beg :token-end])]
   [:line-with-match
    (make-named-matcher #"line=(\S+)"
                        [:query-string] [web-query-to-string])]
   [:paragraph-with-match
    (make-named-matcher #"para=(\S+)"
                        [:query-string] [web-query-to-string])]
   [:org-section-with-match
    (make-named-matcher #"section=(\S+)"
                        [:query-string] [web-query-to-string])]
   ])

(defn parse-link [content-loader link]
  (let [[maybe-protocol maybe-remainder]
        (rest (re-find protocol-matcher link))
        
        protocol (keyword (or maybe-protocol "file"))
        remainder (or maybe-remainder link)
        
        [path maybe-qualifier-separator maybe-qualifier]
        (rest (re-find link-matcher remainder))

        content-resolver (or
                          (when-not (empty? maybe-qualifier)
                            (loop [matcher-remain
                                   (case maybe-qualifier-separator
                                     "::" org-style-range-matchers
                                     "?" url-style-constrictor-matchers
                                     nil)
                                   out nil]
                              (if (or out
                                      (empty? matcher-remain))
                                out
                                (let [[range-type matcher] (first matcher-remain)
                                      maybe-match (matcher maybe-qualifier)]
                                  (recur
                                   (rest matcher-remain)
                                   (if maybe-match
                                     {:bound maybe-match
                                      :type range-type}
                                     out))))))
                          {:type :whole-file})]
    (let [resolved-spec {:link link
                         :path path
                         :protocol protocol
                         :resource-resolver (cond (re-find #"\*" path)
                                                  :glob-name

                                                  (= protocol :grep)
                                                  :grep-content
                                                  
                                                  :else :exact-name)
                         :content-resolver (:type content-resolver)
                         :content-boundary (:bound content-resolver)}
          file-name (corpus/get-file-match
                     (corpus/list-files "_test")
                     resolved-spec)]
      (assoc resolved-spec
             :file-name file-name
             :match-content (some-> resolved-spec
                                    (ci/resolve-content
                                     (content-loader file-name))
                                    (clojure.string/trim))))))

(def transclusion-directive-matcher
  #"\{\{\{transclude\(([^\)]+)\)\}\}\}")

(defn parse-transclusion-directive [text]
  (re-pos transclusion-directive-matcher text))

(defn render-transclusion
  "content-loader should be a function which,
   when passed the :file-name parameter from a `resolved-spec`,
   returns the full text of the target resource (generally a file)"
  [content-loader source-text]
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
            resolved-spec (parse-link content-loader matched-path)]
        (recur (rest remain)
               (+ match-index (count matched-string))
               (conj-if-non-nil
                buffer
                interim-string
                (if-let [rendered-string (:match-content resolved-spec)]
                  (render-transclusion content-loader rendered-string)
                  matched-string)))))))

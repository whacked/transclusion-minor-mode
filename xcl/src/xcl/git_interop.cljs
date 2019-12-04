(ns xcl.git-interop
  (:require [xcl.common :refer [get-file-extension]]
            [xcl.core :as sc]
            [xcl.content-interop :as ci]
            ["fs" :as fs]
            ["path" :as path]
            ["js-yaml" :as yaml]
            ["JSONPath" :as JSONPath]
            ["isomorphic-git" :as git]
            ["orga" :as orga]
            [xcl.node-common :refer
             [path-exists? path-join]]))

(let [git-plugin (aget git "plugins")]
  (js-invoke git-plugin "set""fs" fs))

(def $base-git-param {:fs fs})

(defn buffer-to-string [buffer]
  (.toString buffer))

(defn git-content-object-to-string [content-object]
  (-> content-object
      (aget "object")
      (buffer-to-string)))

;; git ls-files -s README.org
(def $REPO-DIR "/Users/natto/cloudsync/main/devsync/transclusion-minor-mode")
(comment
  ;; EXAMPLE isomorphic-git API
  ;; get historical content for a file
  (let [path-in-repo "README.org"]
    (-> (.log git (clj->js (assoc $base-git-param
                                  :dir $REPO-DIR)))
        (.then (fn [commits]
                 (let [prev-oid (atom nil)
                       total (atom 0)]
                   (doseq [commit-description (->> commits
                                                   (array-seq)
                                                   (sort-by (fn [commit]
                                                              (aget commit "author" "timestamp"))))]
                     (let [{timestamp :timestamp
                            tzoffset :timezoneOffset}
                           (-> (js->clj commit-description :keywordize-keys true)
                               (:author))
                           js-utc (-> (* timestamp 1000)
                                      (+ (* tzoffset 60)))
                           js-commit-date (js/Date. js-utc)]
                       
                       (-> (.readObject git
                                        (clj->js (assoc $base-git-param
                                                        :dir $REPO-DIR
                                                        :oid (aget commit-description "oid")
                                                        :filepath path-in-repo)))
                           (.then (fn [retrieved-object]
                                    (let [retrieved-oid (aget retrieved-object "oid")]
                                      (when (not= retrieved-oid @prev-oid)
                                        (reset! prev-oid retrieved-oid)
                                        (swap! total inc)
                                        
                                        (js/console.log @total
                                                        js-commit-date
                                                        (aget commit-description "message")
                                                        (-> (git-content-object-to-string
                                                             retrieved-object)
                                                            (subs 0 100))))))))))))))))





(defn get-blob-content-from-commit [commit-oid path-in-repo fn-on-success]
  (-> (.readObject git
                   (clj->js (assoc $base-git-param
                                   :dir $REPO-DIR
                                   :oid commit-oid
                                   :filepath path-in-repo)))
      (.then (fn [retrieved-object]
               (-> (git-content-object-to-string
                    retrieved-object)
                   (fn-on-success))))))

(comment
  ;; demonstrate retrieving file for a given commit
  (let [path-in-repo "README.org"
        org-heading "*also see"
        content-resolver (str "::" org-heading)]
    (doseq [commit-rev-to-match ["eb64c0e82c7c" "32a56c9d9d59"]]
      (let [git-resource-address (make-git-resource-address
                                  "." commit-rev-to-match path-in-repo content-resolver)]
        (-> (.log git (clj->js (assoc $base-git-param
                                      :dir $REPO-DIR)))
            (.then (fn [commits]
                     (let [prev-oid (atom nil)
                           total (atom 0)]
                       (doseq [commit-description (->> commits
                                                       (array-seq)
                                                       (sort-by (fn [commit]
                                                                  (aget commit "author" "timestamp"))))]
                         (let [commit-clj-object (js->clj commit-description :keywordize-keys true)
                               {timestamp :timestamp
                                tzoffset :timezoneOffset} (-> commit-clj-object (:author))
                               js-utc (-> (* timestamp 1000)
                                          (+ (* tzoffset 60)))
                               js-commit-date (js/Date. js-utc)

                               commit-oid (:oid commit-clj-object)]

                           (when (clojure.string/starts-with? commit-oid commit-rev-to-match)
                             (get-blob-content-from-commit
                              commit-oid path-in-repo
                              (fn [full-content]
                                (println "~ HIT REV ~" git-resource-address)
                                (js/console.log ">>>" (apply str (take 100 (repeat "-"))) commit-rev-to-match)
                                ;; (-> full-content
                                ;;     ;; (subs 0 200)
                                ;;     (js/console.info))
                                
                                
                                (let [spec (sc/parse-link "./blob/eb64c0e82c7c/README.org::*also see")]
                                  (println (str "*** SPEC ***\n"
                                                (pr-str spec)))
                                  (println (ci/resolve-content
                                            spec full-content)))
                                (js/console.log "<<<" (apply str (take 100 (repeat "-"))) commit-rev-to-match))))))))))))))

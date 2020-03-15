(ns xcl.git-interop
  (:require [xcl.common :refer [get-file-extension]]
            [xcl.core :as sc]
            [xcl.content-interop :as ci]
            ["fs" :as fs]
            ["path" :as path]
            ["js-yaml" :as yaml]
            ["jsonpath-plus" :as JSONPath]
            ["isomorphic-git" :as git]
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

(defn make-git-resource-address [repo-name oid-hash path-in-repo & [resolver-string]]
  ;; github's example
  ;; https://github.com/hubotio/hubot/blob/ed25584f5ac2520a6c28547ffd0961c7abd7ea49/README.md
  (comment
    (make-git-resource-address "hubot" "ed25584f5ac2520a6c28547ffd0961c7abd7ea49" "README.md"))

  (str
   repo-name "/blob"
   "/" oid-hash
   "/" path-in-repo
   (when resolver-string
     (str "::" resolver-string))))

(defrecord GitResourceAddress
    [repo-name oid path content-resolver])

(defn parse-git-resource-address [git-resource-address]
  (comment
    (parse-git-resource-address "./blob/eb64c0e82c7c/README.org::*also see"))
  (when-let [[full-match
              repo-name
              oid-hash
              path-in-repo
              content-resolver]
             (re-find #"([^/]+)/blob/([a-fA-F0-9]+)/([^:?]+)(.*)"
                      git-resource-address)]
    (GitResourceAddress.
     repo-name oid-hash path-in-repo content-resolver)))

(defn parse-git-protocol-blob-path [git-protocol-path]
  (comment
    (parse-git-protocol-blob-path
     "git:../../../.git/blob/e12ac2/xcl/README.org::*content resolvers"))
  (let [known-address-pattern (str "git:(.+?)/blob/([0-9a-fA-F]{5,})/(.+)")
        [_full-match
         repo-path
         commit-oid
         path-in-repo-with-resolver]
        (re-matches
         (re-pattern known-address-pattern)
         git-protocol-path)]
    (when-not _full-match
      (throw (js/Error.
              (str
               "ERROR:\n"
               "  git resolver does not recognize this path notation:\n"
               git-protocol-path "\n"
               "  currently, only this pattern is understood:\n"
               known-address-pattern
               "\n"))))
    (let [[_
           path-in-repo
           content-resolver]
          (re-matches
           #"(.+?)::(.+)"
           path-in-repo-with-resolver)]
      (GitResourceAddress.
       repo-path commit-oid
       (or path-in-repo
           path-in-repo-with-resolver)
       content-resolver))))

(defn load-repo-file-from-commit [repo-dir path-in-repo commit-oid fn-on-success]
  (-> (.readObject git
                   (clj->js (assoc $base-git-param
                                   :dir repo-dir
                                   :oid commit-oid
                                   :filepath path-in-repo)))
      (.then (fn [retrieved-object]
               (-> (git-content-object-to-string
                    retrieved-object)
                   (fn-on-success)))
             (fn [error]
               (js/console.error error)))))

(defn resolve-git-resource-address [{commit-oid :oid
                                     repo-dir :repo-name
                                     path-in-repo :path
                                     resolver :content-resolver
                                     :as GRA}
                                    on-resolved
                                    & [on-failed]]
  {:pre [(instance? GitResourceAddress GRA)]}
  
  (-> (.log git (clj->js (assoc $base-git-param
                                :dir repo-dir)))
      (.then (fn [commits]
               (loop [remain-commits
                      (->> commits
                           (array-seq)
                           (sort-by (fn [commit]
                                      (aget commit "author" "timestamp"))))]
                 
                 (if (empty? remain-commits)
                   (if on-failed
                     (on-failed GRA)
                     (js/console.warn
                      (str "FAILED TO RESOLVE:\n"
                           (pr-str GRA))))
                   (let [commit-description (first remain-commits)
                         commit-clj-object (js->clj commit-description :keywordize-keys true)
                         {timestamp :timestamp
                          tzoffset :timezoneOffset} (-> commit-clj-object (:author))]
                     (if-not (clojure.string/starts-with?
                              (:oid commit-clj-object)
                              commit-oid)
                       (recur (rest remain-commits))
                       (load-repo-file-from-commit
                        repo-dir
                        path-in-repo
                        (:oid commit-clj-object)
                        on-resolved)))))))))

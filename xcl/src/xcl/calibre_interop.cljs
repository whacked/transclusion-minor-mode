(ns xcl.calibre-interop
  (:require ["sqlite3" :as sqlite3]
            ["fs" :as fs]
            [xcl.core :as sc]
            [xcl.content-interop :as ci]
            [xcl.sqlite :as sqlite]
            [xcl.node-epub-interop :as epubi]
            [xcl.node-common :refer
             [path-exists? path-join]]))

(def $user-profile-data-candidates
  (let [process-user (aget js/process "env" "USER")]
   [(aget js/process "env" "APPDATA")
    (aget js/process "env" "USERPROFILE")
    (aget js/process "env" "HOME")
    (path-join "/home" process-user)
    (path-join "/Users" process-user)]))

(def $calibre-library-directory
  (or (if-let [global-py-json
               (some->> $user-profile-data-candidates
                        (filter identity)
                        (map
                         (fn [base-dir]
                           (path-join
                            base-dir
                            ".config"
                            "calibre"
                            "global.py.json")))
                        (filter path-exists?)
                        (first))]
        (some-> (.readFileSync fs global-py-json "utf-8")
                (js/JSON.parse)
                (aget "library_path")))
      (some->> $user-profile-data-candidates
               (filter identity)
               (map (fn [base-dir]
                      (path-join base-dir "Calibre Library")))
               (filter path-exists?)
               (first))))

(if $calibre-library-directory
  (do
    (def $calibre-db-path
      (path-join $calibre-library-directory "metadata.db"))
    (def $calibre-db (new sqlite3/Database $calibre-db-path))
    (js/console.info (str "REGISTERING LOADER FOR calibre; storage detected at " $calibre-db-path)))
  (do
    (js/console.error "ERROR: could not detect calibre library path")
    (def $calibre-db-path nil)
    (def $calibre-db nil)))

(defn calibre-get-book-filepath
  [book-dir book-name book-format]
  (js/console.log "GET PATH")
  (path-join
   $calibre-library-directory
   book-dir
   (str book-name
        "."
        (clojure.string/lower-case book-format))))

(defn find-matching-epub
  [epub-search-string
   on-found-file
   & [on-not-exist]]
  (let [calibre-query (sqlite/filename-glob-to-query epub-search-string)]
    (-> $calibre-db
        (.all
         (aget sqlite/$sql "calibreBuildDefaultQuery")
         calibre-query
         calibre-query
         1 ;; limit
         (fn [err js-rows]
           (when err (js/console.error err))
           (when-let [book (-> js-rows
                               (js->clj :keywordize-keys true)
                               (first))]
             (let [book-filepath (calibre-get-book-filepath
                                  (:path book)
                                  (:name book)
                                  (:format book))]
               (if-not (path-exists? book-filepath)
                 (if on-not-exist
                   (on-not-exist book-filepath)
                   (js/console.error
                    (str "FILE AT [" book-filepath "] DOES NOT EXIST")))
                 (on-found-file book-filepath)))))))))

(defn load-text-from-epub
  [epub-search-string
   target-string-or-spec
   on-found-text
   & [on-error]]
  (find-matching-epub
   epub-search-string
   (fn [file-path]
     (epubi/load-and-get-text
      file-path
      nil
      nil
      (fn [{:keys [chapter section text]}]
        (when-let [matched-text
                   (if (string? target-string-or-spec)
                     ;; find tokens directly
                     (let [tokens (clojure.string/split
                                   target-string-or-spec
                                   #"\s+")
                           maybe-matches
                           (sc/find-successive-tokens-in-content
                            text tokens)]
                       (when (= (count maybe-matches)
                                (count tokens))
                         (subs
                          text
                          (:index (first maybe-matches))
                          (+ (:index (last maybe-matches))
                             (:length (last maybe-matches))))))
                     
                     ;; use specification based resolution
                     (ci/resolve-content
                      target-string-or-spec text))]
          
          (js/console.info
           (str "[calibre test OK]\n"
                "    section: " section "\n"
                "    chapter: " chapter "\n"
                ;; expanded full match
                "    "
                matched-text))
          (on-found-text matched-text)))
      nil))))

(ns xcl.calibre-interop
  (:require ["sqlite3" :as sqlite3]
            ["fs" :as fs]
            [xcl.core :as sc]
            [xcl.sqlite :as sqlite]
            [xcl.node-epub-interop :as epubi]
            [xcl.node-common :refer
             [path-exists? path-join]]))

(def $user-profile-data-candidates [(aget js/process "env" "APPDATA")
                                    (aget js/process "env" "USERPROFILE")
                                    (aget js/process "env" "HOME")])

(def $calibre-library-directory
  (or (if-let [global-py (some->> $user-profile-data-candidates
                                  (filter identity)
                                  (map
                                   (fn [base-dir]
                                     (path-join
                                      base-dir "calibre" "global.py")))
                                  (filter path-exists?)
                                  (first))]
        (some->> (.readFileSync fs global-py "utf-8")
                 (clojure.string/split-lines)
                 (filter (fn [line]
                           (re-find #"^\s*library_path" line)))
                 (map (fn [match]
                        (some-> (clojure.string/replace
                                 match
                                 #"\s*(library_path)\s*=.*?['\"]([^'\"]+)['\"]\s*"
                                 "$2")
                                (clojure.string/replace #"\\+" "/"))))
                 
                 (first)))
      (some->> $user-profile-data-candidates
               (filter identity)
               (map (fn [base-dir]
                      (path-join base-dir "Calibre Library")))
               (filter path-exists?)
               (first))))

(when-not $calibre-library-directory
  (js/console.error "ERROR: could not detect calibre library path"))

(def $calibre-db-path
  (path-join $calibre-library-directory "metadata.db"))

(def $calibre-db (new sqlite3/Database $calibre-db-path))

(defn calibre-get-book-filepath
  [book-dir book-name book-format]
  (js/console.log "GET PATH")
  (path-join
   $calibre-library-directory
   book-dir
   (str book-name
        "."
        (clojure.string/lower-case book-format))))

(defn load-epub
  [epub-search-string target-string on-found-text & [on-error]]
  (let [calibre-query (sqlite/filename-glob-to-query epub-search-string)
        on-found-file (fn [file-path]
                        (epubi/load-and-get-text
                         file-path
                         nil
                         nil
                         (fn [{:keys [chapter section text]}]
                           (let [tokens (clojure.string/split target-string #"\s+")
                                 maybe-matches
                                 (sc/find-successive-tokens-in-content
                                  text tokens)]
                             (when (= (count maybe-matches)
                                      (count tokens))
                               (js/console.info
                                (str "[calibre test OK]\n"
                                     "    section: " section "\n"
                                     "    chapter: " chapter "\n"
                                     ;; expanded full match
                                     "    "
                                     (subs
                                      text
                                      (:index (first maybe-matches))
                                      (+ (:index (last maybe-matches))
                                         (:length (last maybe-matches)))))))))
                         on-found-text))]
    (-> $calibre-db
        (.all
         (aget sqlite/$sql "calibreBuildDefaultQuery")
         calibre-query
         calibre-query
         2 ;; limit
         (fn [err js-rows]
           (when err (js/console.error err))
           (when-let [rows (js->clj js-rows :keywordize-keys true)]
             (let [book (first rows)
                   book-filepath (calibre-get-book-filepath
                                  (:path book)
                                  (:name book)
                                  (:format book))]
               (if-not (path-exists? book-filepath)
                 (js/console.error
                  (str "FILE AT [" book-filepath "] DOES NOT EXIST"))
                 (on-found-file book-filepath)))))))))

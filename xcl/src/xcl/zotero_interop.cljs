(ns xcl.zotero-interop
  (:require ["sqlite3" :as sqlite3]
            [xcl.node-common :refer
             [path-exists? path-join]]
            [xcl.core :as sc]
            [xcl.content-interop :as ci]
            [xcl.sqlite :as sqlite]
            [xcl.external :as ext]
            [xcl.pdfjslib-interop :as pdfjslib]))

(def $zotero-library-directory
  (let [process-user (aget js/process "env" "USER")]
   (loop [remain
          [(aget js/process "env" "USERPROFILE") ;; windows
           (aget js/process "env" "HOME")
           (path-join "/home" process-user)
           (path-join "/Users" process-user)]
          maybe-zotero-dir nil]
     (if (or (empty? remain)
             (and maybe-zotero-dir
                  (path-exists? maybe-zotero-dir)))
       maybe-zotero-dir
       (recur
        (rest remain)
        (when-let [candidate (first remain)]
          (path-join candidate "Zotero")))))))

(def $zotero-db-path
  (path-join $zotero-library-directory "zotero.sqlite"))

(when-not $zotero-library-directory
  (js/console.error "ERROR: could not detect zotero library path"))

(def $zotero-db (new sqlite3/Database $zotero-db-path))

(defn zotero-get-attachment-key-path-filepath
  [attachment-key attachment-path]
  (when (and attachment-key attachment-path)
    (path-join
     $zotero-library-directory
     "storage"
     attachment-key
     (clojure.string/replace attachment-path #"^storage:" ""))))

(defn find-matching-file
  [file-search-string
   on-found-file
   & [on-not-exist]]
  (let [zotero-query (sqlite/filename-glob-to-query file-search-string)]
    (-> $zotero-db
        (.all
         (aget sqlite/$sql "zoteroQueryByAttributes")
         zotero-query
         1 ;; limit
         (fn [err js-rows]
           (when err (js/console.error err))
           (when-let [rows (js->clj js-rows :keywordize-keys true)]
             (let [{:keys [attachmentKey
                           attachmentPath]} (first rows)
                   file-path (zotero-get-attachment-key-path-filepath
                              attachmentKey attachmentPath)
                   ]
               (if-not (path-exists? file-path)
                 (if on-not-exist
                   (on-not-exist file-path)
                   (js/console.error
                    (str "FILE AT [" file-path "] DOES NOT EXIST")))
                 (on-found-file file-path)))))))))

(defn load-text-from-html
  []
  (throw (js/Error. "TODO")))

(defn load-text-from-pdf
  [pdf-search-string
   target-string-or-spec
   on-found-text
   & [on-error]]
  (find-matching-file
   pdf-search-string
   (fn [file-path]
     (pdfjslib/process-pdf
      file-path
      (fn on-get-page-text [page-number text]
        {:page page-number
         :text text})
      (fn on-complete-page-texts [pagetexts]
        (let [_page-number-with-string
              (some->> pagetexts
                       (filter
                        (fn [{:keys [page text]}]
                          (if (string? target-string-or-spec)
                            ;; find tokens directly
                            (let [tokens (clojure.string/split
                                          target-string-or-spec #"\s+")
                                  maybe-matches
                                  (sc/find-successive-tokens-in-content
                                   text tokens)]
                              (when (= (count maybe-matches)
                                       (count tokens))
                                (let [match-text
                                      (subs
                                       text
                                       (:index (first maybe-matches))
                                       (+ (:index (last maybe-matches))
                                          (:length (last maybe-matches))))]
                                  (on-found-text page match-text))
                                page))

                            ;; use specification based resolution
                            (when-let [resolved-text (ci/resolve-content
                                                      target-string-or-spec
                                                      text)]
                              (js/console.info
                               (str "[zotero RANGE TEST OK]\n"
                                    "    page: " page "\n"
                                    "    "
                                    (pr-str target-string-or-spec)))
                              (on-found-text page resolved-text)))))
                       (first)
                       (:page))]
          _page-number-with-string))
      (or on-error identity)))))

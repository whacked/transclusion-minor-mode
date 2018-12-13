(ns xcl.test
  (:require ["sqlite3" :as sqlite3]
            ["yesql" :as yesql]
            ["fs" :as fs]
            ["path" :as path]))

(def calibre-library-directory
  "/path/to/Calibre Library")
(def calibre-db
  (new sqlite3/Database
       path-to-metadata.db))

(def zotero-library-directory
  "/path/to/Zotero")
(def zotero-db
  (new sqlite3/Database
       path-to-zotero.sqlite))

(defn zotero-get-attachment-key-path-filepath
  [attachment-key attachment-path]
  (.join path
         zotero-library-directory
         "storage"
         attachment-key
         (clojure.string/replace attachment-path #"^storage:" "")))

(defn calibre-get-book-filepath
  [book-dir book-name book-format]
  (.join path
         calibre-library-directory
         book-dir
         (str book-name
              "."
              (clojure.string/lower-case book-format))))

(def sql
  (yesql (.join path (.cwd js/process) "src")))

(defn filename-glob-to-query [filename]
  (-> path
      (.parse filename)
      (aget "name")
      (clojure.string/lower-case)
      (clojure.string/replace #"\*" "%")))

(let [calibre-search "QuIcK sTaRt*.epub"
      calibre-query (filename-glob-to-query calibre-search)]
  (-> calibre-db
      (.all
       (aget sql "calibreBuildDefaultQuery")
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
             (if-not (.existsSync fs book-filepath)
               (js/console.error
                (str "FILE AT [" book-filepath "] DOES NOT EXIST"))
               (println "BOOK FOUND AT"
                        book-filepath))))))))

(let [zotero-search "Trace-based just-in-time*.pdf"
      zotero-query (filename-glob-to-query zotero-search)]
  (-> zotero-db
      (.all
       (aget sql "zoteroQueryByAttributes")
       zotero-query
       2 ;; limit
       (fn [err js-rows]
         (when err (js/console.error err))
         (when-let [rows (js->clj js-rows :keywordize-keys true)]
           (let [{:keys [attachmentKey
                         attachmentPath]} (first rows)
                 file-path (zotero-get-attachment-key-path-filepath
                            attachmentKey attachmentPath)
                 ]
             (if-not (.existsSync fs file-path)
               (js/console.error
                (str "FILE AT [" file-path "] DOES NOT EXIST"))
               (println "ARTICLE FOUND AT"
                file-path))))))))

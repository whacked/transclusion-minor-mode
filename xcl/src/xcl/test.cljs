(ns xcl.test
  (:require ["sqlite3" :as sqlite3]
            ["yesql" :as yesql]
            ["fs" :as fs]
            ["path" :as path]
            [xcl.core :as sc]
            [xcl.external :as ext]
            [xcl.pdfjslib-interop :as pdfjslib]
            [xcl.node-epub-interop :as epubi]
            [xcl.node-interop :as ni]))

(def all-tests (atom 0))

(defn signal-test-done! []
  (swap! all-tests dec)
  (when (<= @all-tests 0)
    (js/process.exit)))

(defn path-exists? [p]
  (.existsSync fs p))

(defn path-join [& ps]
  (apply (aget path "join") ps))

(def $user-profile-data-candidates [(aget js/process "env" "APPDATA")
                                    (aget js/process "env" "USERPROFILE")
                                    (aget js/process "env" "HOME")])

(def $calibre-library-directory
  (or (if-let [global-py (some->> $user-profile-data-candidates
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
               (map (fn [base-dir]
                      (path-join base-dir "Calibre Library")))
               (filter path-exists?)
               (first))))

(when $calibre-library-directory
  (js/console.error "ERROR: could not detect calibre library path"))

(def $calibre-db-path
  (path-join $calibre-library-directory "metadata.db"))

(def $calibre-db (new sqlite3/Database $calibre-db-path))

(def $zotero-library-directory
  (let [maybe-zotero-dir
        (if-let [windows-user-profile (aget js/process "env" "USERPROFILE")]
          (path-join windows-user-profile "Zotero")
          (path-join (aget js/process "env" "HOME") "Zotero"))]
    (when (path-exists? maybe-zotero-dir)
      maybe-zotero-dir)))

(def $zotero-db-path
  (path-join $zotero-library-directory "zotero.sqlite"))

(when $zotero-library-directory
  (js/console.error "ERROR: could not detect zotero library path"))

(def $zotero-db (new sqlite3/Database $zotero-db-path))

(defn zotero-get-attachment-key-path-filepath
  [attachment-key attachment-path]
  (path-join
   $zotero-library-directory
   "storage"
   attachment-key
   (clojure.string/replace attachment-path #"^storage:" "")))

(defn calibre-get-book-filepath
  [book-dir book-name book-format]
  (path-join
   $calibre-library-directory
   book-dir
   (str book-name
        "."
        (clojure.string/lower-case book-format))))

(def sql
  (yesql (path-join (.cwd js/process) "src")))

(defn filename-glob-to-query [filename]
  (-> path
      (.parse filename)
      (aget "name")
      (clojure.string/lower-case)
      (clojure.string/replace #"\*" "%")))

(defn zotero-test []
  (let [zotero-search "Trace-based just-in-time*.pdf"
        zotero-query (filename-glob-to-query zotero-search)
        target-string "Monkey observes that so TraceMonkey attempts"

        pdf-loader (@ext/$ExternalLoaders "pdf")

        on-found-file (fn [file-path]
                        (let [rel-uri (str "file:///" file-path)
                              getDocument (aget @pdfjslib/pdfjsLib "getDocument")]
                          (js-invoke
                           (getDocument rel-uri)
                           "then"
                           (fn [pdf]
                             (let [count-promises (clj->js [])
                                   page-beg 1
                                   page-end (aget pdf "numPages")]
                               (doseq [n (range page-beg (inc page-end))]
                                 (let [page (js-invoke pdf "getPage" n)]
                                   (.push count-promises
                                          (js-invoke
                                           page
                                           "then"
                                           (fn [p]
                                             (-> p
                                                 (js-invoke "getTextContent")
                                                 (js-invoke "then"
                                                            (fn [text]
                                                              {:page n
                                                               :text (-> (aget text "items")
                                                                         (.map (fn [s]
                                                                                 (aget s "str")))
                                                                         (.join " "))}))))))))
                               
                               (-> js/Promise
                                   (.all count-promises)
                                   (.then (fn [pagetexts]
                                            (let [_page-number-with-string
                                                  (some->> pagetexts
                                                           (filter
                                                            (fn [{:keys [page text]}]
                                                              (let [tokens (clojure.string/split target-string #"\s+")
                                                                    maybe-matches
                                                                    (sc/find-successive-tokens-in-content
                                                                     text tokens)]
                                                                (when (= (count maybe-matches)
                                                                         (count tokens))
                                                                  (js/console.log
                                                                   (str "[zotero test OK]\n"
                                                                        "    page: " page "\n"
                                                                        "    "
                                                                        (subs
                                                                         text
                                                                         (:index (first maybe-matches))
                                                                         (+ (:index (last maybe-matches))
                                                                            (:length (last maybe-matches))))
                                                                        "\n\n"))
                                                                  page))))
                                                           (first)
                                                           (:page))]
                                              _page-number-with-string)
                                            (signal-test-done!)))
                                   (.catch (fn [err]
                                             (js/console.error err)
                                             (signal-test-done!)))))))))]
    (-> $zotero-db
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
               (if-not (path-exists? file-path)
                 (js/console.error
                  (str "FILE AT [" file-path "] DOES NOT EXIST"))
                 (on-found-file file-path)))))))))

(defn calibre-test []
  (let [calibre-search "QuIcK sTaRt*.epub"
        calibre-query (filename-glob-to-query calibre-search)
        target-string "Calibre display possible matches for entered"
        
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
                               (js/console.log
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
                         (fn [text]
                           (js/console.log
                            (str "    calibre epub: " (count text) " bytes\n\n"))
                           (signal-test-done!))))]
    (-> $calibre-db
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
               (if-not (path-exists? book-filepath)
                 (js/console.error
                  (str "FILE AT [" book-filepath "] DOES NOT EXIST"))
                 (on-found-file book-filepath)))))))))

(defn pdf-loader-test []
  (let [external-loader (@ext/$ExternalLoaders "pdf")
        resource-spec (sc/parse-link 
                       "xcl:./public/tracemonkey.pdf?p=3&s=Monkey observes that...so TraceMonkey attempts")]
    (external-loader
     resource-spec
     (fn [text]
       (js/console.log (str "[PDF OK]\n"
                            "    " (pr-str resource-spec) "\n"
                            "    " (-> text
                                       (clojure.string/trim)
                                       (subs 0 200))
                            "\n\n"))
       (signal-test-done!)))))

(defn epub-loader-test []
  (let [external-loader (@ext/$ExternalLoaders "epub")
        resource-spec (sc/parse-link
                       "xcl:./public/alice.epub?p=2-&s=Would you tell me, please...walk long enough")]
    (external-loader
     resource-spec
     (fn [text]
       (js/console.log (str "[EPUB OK]\n"
                            "    " (pr-str resource-spec) "\n"
                            "    " (-> text
                                       (clojure.string/trim)
                                       (subs 0 200))
                            "\n\n"))
       (signal-test-done!)))))

(defn add-node-test! [test-func]
  (swap! all-tests inc)
  (test-func))

(defn -main []
  (js/console.log
   (str "### script running from dir\n"
        "    " js/__dirname "\n"
        "### process running from dir\n"
        "    " (.cwd js/process)
        "\n\n"))
  
  (when $zotero-library-directory
    (add-node-test! zotero-test))
  (when $calibre-library-directory
    (add-node-test! calibre-test))
  (add-node-test! pdf-loader-test)
  (add-node-test! epub-loader-test))

(-main)

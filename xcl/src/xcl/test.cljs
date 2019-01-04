(ns xcl.test
  (:require ["sqlite3" :as sqlite3]
            ["yesql" :as yesql]
            [xcl.core :as sc]
            [xcl.node-common :refer
             [path-join]]
            [xcl.external :as ext]
            [xcl.calibre-interop :as calibre]
            [xcl.zotero-interop :as zotero]))

(def all-tests (atom 0))

(defn signal-test-done! []
  (swap! all-tests dec)
  (when (<= @all-tests 0)
    (js/process.exit)))

(defn zotero-test []
  (zotero/load-pdf
   "Trace-based just-in-time*.pdf"
   "Monkey observes that so TraceMonkey attempts"
   (fn [page text]
     (js/console.info
      (str "[zotero test OK]\n"
           "    page: " page "\n"
           "    "
           text
           "\n\n"))
     (signal-test-done!))
   (fn [err]
     (js/console.error err)
     (signal-test-done!))))

(defn calibre-test []
  (calibre/load-epub
   "QuIcK sTaRt*.epub"
   "Calibre display possible matches for entered"
   (fn [text]
     (js/console.log
      (str "    calibre epub: " (count text) " bytes\n\n"))
     (signal-test-done!))))

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
                       "xcl:./public/alice.epub?p=2&s=Would you tell me, please...walk long enough")]
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
  
  (when zotero/$zotero-library-directory
    (add-node-test! zotero-test))
  (when calibre/$calibre-library-directory
    (add-node-test! calibre-test))
  (add-node-test! pdf-loader-test)
  (add-node-test! epub-loader-test))

(-main)

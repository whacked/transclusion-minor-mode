(ns xcl.test
  (:require ["fs" :as fs]
            [xcl.core :as sc]
            [xcl.content-interop :as ci]
            [xcl.node-interop :as ni]
            [xcl.node-common :refer
             [path-join]]
            [xcl.external :as ext]
            [xcl.calibre-interop :as calibre]
            [xcl.zotero-interop :as zotero]))

(def all-tests (atom []))

(defn nocolor [& ss]
  (str (apply str ss) "\033[0m"))

(defn red [s]
  (js/console.log (nocolor "\033[31m" s)))
(defn green [s]
  (js/console.log (nocolor "\033[32m" s)))
(defn yellow [s]
  (js/console.log (nocolor "\033[33m" s)))
(defn blue [s]
  (js/console.log (nocolor "\033[34m" s)))
(defn magenta [s]
  (js/console.log (nocolor "\033[36m" s)))
(defn cyan [s]
  (js/console.log (nocolor "\033[36m" s)))

(defn slurp [path]
  (.readFileSync fs path "utf-8"))

(def $XCL-SERVER-RESOURCE-BASE-DIR
  (path-join
   (.cwd js/process) ".."))

(defn get-resource-path
  [file-name]
  (path-join
   $XCL-SERVER-RESOURCE-BASE-DIR
   file-name))

(defn run-all-tests! []
  (when-let [test-func (first @all-tests)]
    (swap! all-tests rest)
    (test-func)))

(defn signal-test-done! []
  (magenta
   (str (count @all-tests) " tests remaining..."))
  (if (<= (count @all-tests) 0)
    (js/process.exit)
    (run-all-tests!)))

(defn zotero-test-pdf []
  (zotero/load-text-from-file
   "Trace-based just-in-time*.pdf"
   "Monkey observes that so TraceMonkey attempts"
   (fn [page text]
     (green "[zotero test pdf OK]")
     (js/console.info
      (str "    page: " page "\n"
           "    "
           text
           "\n\n"))
     (signal-test-done!))
   (fn [err]
     (js/console.error err)
     (signal-test-done!))))

(defn zotero-test-html []
  ;; from example.com
  (zotero/load-text-from-file
   "*Example Domain*.html"
   "examples without prior coordination or asking"
   (fn [text]
     (green "[zotero test html OK]")
     (js/console.info
      (str "    "
           text
           "\n\n"))
     (signal-test-done!))
   (fn [err]
     (js/console.error err)
     (signal-test-done!))))

(defn calibre-test []
  (calibre/load-text-from-epub
   "QuIcK sTaRt*.epub"
   "Calibre display possible matches for entered"
   (fn [text]
     (green "[calibre OK]")
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
       (green "[PDF OK]")
       (js/console.log (str "    " (pr-str resource-spec) "\n"
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
       (green "[EPUB OK]")
       (js/console.log (str "    " (pr-str resource-spec) "\n"
                            "    " (-> text
                                       (clojure.string/trim)
                                       (subs 0 200))
                            "\n\n"))
       (signal-test-done!)))))

(defn add-node-test! [test-func]
  (cyan ">>>>>>>>> adding a test!")
  (swap! all-tests conj test-func)
  (blue (str " ----- added a test; now " (count @all-tests))))

(defn -main []
  (js/console.log
   (str "### script running from dir\n"
        "    " js/__dirname "\n"
        "### process running from dir\n"
        "    " (.cwd js/process)
        "\n\n"))

  (defn load-from-directive [directive]
    (let [spec (sc/parse-link directive)
          full-content (slurp
                        (get-resource-path
                         (:resource-resolver-path spec)))]
      (yellow
       (str "*** SPEC ***\n"
            (:resource-resolver-path spec) "\n"
            (pr-str spec)))
      (green
       (str "\n"
            "========================================"))
      (println (ci/resolve-content
                spec full-content))
      (green "----------------------------------------")))
  
  (add-node-test!
   (fn default-file []
     (let [directive "transcluding-org-elements.org"]
       (load-from-directive directive)
       (signal-test-done!))))

  (add-node-test!
   (fn fs-file []
     (let [directive "LICENSE::2-10"]
       (load-from-directive directive)
       (signal-test-done!))))

  (add-node-test!
   (fn fs-file-with-line-range []
     (let [directive "file:transcluding-org-elements.org::153,185"]
       (load-from-directive directive)
       (signal-test-done!))))

  (add-node-test!
   (fn fs-file-with-org-heading []
     (let [directive "file:README.org::*line range"]
       (load-from-directive directive)
       (signal-test-done!))))

  (add-node-test!
   (fn fs-file-with-token-bound []
     (let [directive "file:README.org::There is 1 goal...a range resource"]
       (load-from-directive directive)
       (signal-test-done!))))

  (add-node-test!
   (fn xcl-first-token-range []
     (let [directive "xcl:README.org?s=There is 1 goal...a range resource"]
       (load-from-directive directive)
       (signal-test-done!))))

  (add-node-test!
   (fn xcl-first-matching-paragraph []
     (let [directive "xcl:README.org?para=the+pipe+is+an+obvious"]
       (load-from-directive directive)
       (signal-test-done!))))
  
  (add-node-test!
   (fn xcl-yaml-jsonpath []
     (let [directive "xcl:xcl/public/test-highlight-file.yml?jsonpath=$.highlights[2].highlightText"]
       (load-from-directive directive)
       (signal-test-done!))))

  (when zotero/$zotero-library-directory
    (add-node-test! zotero-test-pdf)
    (add-node-test! zotero-test-html))
  
  (when calibre/$calibre-library-directory
    (add-node-test! calibre-test))
  
  (add-node-test! pdf-loader-test)
  
  (add-node-test! epub-loader-test)
  
  (run-all-tests!))

(-main)

(ns xcl.node-webserver
  (:require
   ["express" :as express]
   ["body-parser" :as body-parser]
   ["multitransport-jsonrpc" :as jsonrpc]
   [xcl.common :refer [get-file-extension]]
   
   ;; from test.cljs
   ["sqlite3" :as sqlite3]
   ["yesql" :as yesql]
   ["fs" :as fs]
   ["path" :as path]
   ["child_process" :as child-process]
   [xcl.core :as sc]
   [xcl.content-interop :as ci]
   [xcl.external :as ext]
   [xcl.pdfjslib-interop :as pdfjslib]
   [xcl.node-epub-interop :as epubi]
   [xcl.node-interop :as ni]
   [xcl.node-common :refer
    [path-join path-exists? slurp-file]]
   [xcl.calibre-interop :as calibre]
   [xcl.zotero-interop :as zotero]))

(def $JSONRPC-PORT
  (let [config-file-path (path-join
                          (js/process.cwd)"config.json")]
    (or
     (when (path-exists? config-file-path)
       (-> config-file-path
           (slurp-file)
           (js/JSON.parse)
           (aget "jsonrpc-port")))
     23120)))

(def $resource-resolver-loader-mapping
  (atom {:calibre-file
         (fn [spec callback]
           (calibre/load-text-from-epub
            (str "*" (:resource-resolver-path spec) "*.epub")
            spec
            (fn [text]
              (js/console.log
               (str "calibre epub: "
                    (count text)
                    " bytes\n\n"))
              (->> text
                   (clojure.string/trim)
                   (assoc spec :text)
                   (clj->js)
                   (callback nil)))))

         :zotero-file
         (fn [spec callback]
           (zotero/load-text-from-file
            (str "*" (:resource-resolver-path spec) "*")
            spec
            (fn [text & [page]]
              (js/console.log
               (str "zotero loaded:\n"
                    (when page
                      (str "page: " page "\n"))
                    (count text)
                    " bytes\n\n"))
              (callback
               nil
               (->> (if text (clojure.string/trim text) "")
                    (assoc spec :text)
                    (clj->js)
                    (callback nil))))
            (fn [err]
              (js/console.error err))))

         :exact-name
         (fn [spec callback]
           (let [extension (get-file-extension
                            (:resource-resolver-path
                             spec))]
             (when-let [external-loader (@ext/$ExternalLoaders extension)]
               (println "loading for extension " extension
                        "\n" spec)
               (external-loader
                spec
                (fn [text]
                  (some->> (ci/resolve-content spec text)
                           (clojure.string/trim)
                           (assoc spec :text)
                           (clj->js)
                           (callback nil)))))))}))

(defn load-by-resource-resolver [spec callback]
  (when-let [loader (@$resource-resolver-loader-mapping
                     (:resource-resolver-method spec))]
    (loader spec callback)))

(defn open-file-natively [file-path]
  (let [open-command (str
                      (case (aget js/process "platform")
                        "win64" "open"
                        "darwin" "open"
                        "xdg-open")
                      " "
                      "\""
                      file-path
                      "\"")]
    (println "INVOKE: " open-command)
    (js-invoke
     child-process "exec" open-command)))

(def $handler-mapping
  {:echo (fn [args callback]
           (println (js->clj args :keywordize-keys true))
           (-> args (js->clj) (println))
           (callback nil args))
   
   :get-text (fn [args callback]
               (let [{:keys [protocol directive]}
                     (js->clj args :keywordize-keys true)]
                 ((case protocol

                    ("file" "xcl")
                    (fn [directive callback]
                      ;; directive is e.g.
                      ;; "xcl:./public/tracemonkey.pdf?p=3&s=Monkey observes that...so TraceMonkey attempts"
                      (let [resource-spec (sc/parse-link directive)
                            resolved-resource-path (:resource-resolver-path
                                                    resource-spec)
                            extension (get-file-extension
                                       resolved-resource-path)
                            resolve-content-and-return!
                            (fn [text]
                              (some->> (ci/resolve-content resource-spec text)
                                       (clojure.string/trim)
                                       (assoc resource-spec :text)
                                       (clj->js)
                                       (callback nil)))]
                        (println "loading for extension " extension
                                 "\n" resource-spec)
                        (if-let [external-loader (@ext/$ExternalLoaders extension)]
                          (external-loader
                           resource-spec
                           resolve-content-and-return!)

                          (.readFile fs
                                     resolved-resource-path
                                     "utf-8"
                                     (fn [err text]
                                       (resolve-content-and-return! text))))))

                    ("calibre" "zotero")
                    (fn [directive callback]
                      (let [resource-spec (sc/parse-link directive)]
                        (load-by-resource-resolver
                         resource-spec
                         callback))))
                  
                  directive callback)))
   
   :open (fn [args callback]
           (let [{:keys [protocol directive]}
                 (js->clj args :keywordize-keys true)
                 resource-spec (sc/parse-link directive)
                 resolved-path (:resource-resolver-path
                                resource-spec)
                 complete-request
                 (fn [file-path]
                   (->> {:status (or (when (path-exists? file-path)
                                       (open-file-natively
                                        file-path)
                                       "ok")
                                     "error")}
                        (clj->js)
                        (callback nil)))]
             
             (case protocol
               "calibre"
               (calibre/find-matching-epub
                (str "*" resolved-path "*.epub")
                complete-request)
               
               "zotero"
               (zotero/find-matching-file
                (str "*" resolved-path "*")
                complete-request)
               
               ;; generic
               (complete-request resolved-path))))
   })

(defn start-server! []
  (let [app (express)
        JrpcServer (aget jsonrpc "server")
        ServerMiddleware (aget jsonrpc
                               "transports"
                               "server"
                               "middleware")]
    (js/console.log
     (str "starting rpc server on port "
          $JSONRPC-PORT
          "..."))
    (doto app
      (.use (js-invoke body-parser "json"))
      (.use "/rpc"
            (aget (JrpcServer.
                   (ServerMiddleware.)
                   (clj->js $handler-mapping))
                  "transport"
                  "middleware"))
      (.listen $JSONRPC-PORT))))

(defn -main []
  (start-server!))

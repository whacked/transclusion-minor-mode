(ns xcl.node-webserver
  (:require
   ["express" :as express]
   ["cors" :as cors]
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
   [xcl.zotero-interop :as zotero]
   [xcl.git-interop :as git]
   [xcl.console :as console]
   [xcl.env :as env]))

(def $JSONRPC-PORT (env/get :jsonrpc-port))

(def $resource-resolver-loader-mapping
  (atom {:calibre-file
         (fn [spec callback]
           (calibre/load-text-from-epub
            (str "*" (:resource-resolver-path spec) "*.epub")
            spec
            (fn [text]
              (js/console.log (str "calibre epub: " (count text) " bytes\n\n"))
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
              (->> (if text (clojure.string/trim text) "")
                   (assoc spec :text)
                   (clj->js)
                   (callback nil)))
            (fn [err]
              (js/console.error err))))

         :exact-name
         (fn [spec callback]
           (let [extension (get-file-extension
                            (:resource-resolver-path
                             spec))]
             (when-let [external-loader (@ext/$ExternalLoaders extension)]
               (println "!!! loading for extension " extension
                        "\n" spec " --using--> " external-loader)
               (external-loader
                spec
                (fn [text]
                  (some->> (ci/resolve-content spec text)
                           (clojure.string/trim)
                           (assoc spec :text)
                           (clj->js)
                           (callback nil)))))))}))

(defn load-by-resource-resolver [spec callback]
  (if-let [loader (@$resource-resolver-loader-mapping
                   (:resource-resolver-method spec))]
    (loader spec callback)
    (do
      (js/console.warn (console/red "FAILED TO LOAD RESOLVER FOR "
                                    (str spec))
                       " available resolvers:")
      (doseq [key (keys @$resource-resolver-loader-mapping)]
        (js/console.warn (str "- " key))))))

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

(def $request-cache (atom {}))
(defn cache-log [& ss]
  (js/console.info
   (apply str
          (console/cyan "[CACHE-CONTROL] ")
          ss)))

(defn wrap-cached [req-mapping]
  (->> req-mapping
       (map (fn [[method-key original-handler]]
              (cache-log "wrapping request cache for " method-key)
              [method-key
               (fn [args original-callback]
                 (let [cache-key (pr-str args)
                       maybe-cached-response (@$request-cache cache-key)]
                   (cache-log "checking cache key: " cache-key)
                   (if-not (empty? maybe-cached-response)
                     (do
                       (cache-log (console/green "returning cached response for " cache-key))
                       (apply original-callback maybe-cached-response))
                     (do
                       (cache-log (console/red "running original handler for " cache-key))
                       (original-handler
                        args
                        (fn wrapped-callback [err-response ok-response]
                          (cache-log (console/yellow "caching response for " cache-key))
                          (swap! $request-cache
                                 assoc cache-key [err-response ok-response])
                          (original-callback err-response ok-response)))))))]))
       (into {})))

;;add jsonrpc request cache wrapper fn
(def $handler-mapping
  (wrap-cached
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
                                        (assoc resource-spec :text)
                                        (clj->js)
                                        (callback nil)))]
                         (println "loading for extension " extension
                                  "\n" resource-spec)
                         (if-let [external-loader (@ext/$ExternalLoaders extension)]
                           (external-loader
                            resource-spec
                            resolve-content-and-return!)

                           (if-not (path-exists? resolved-resource-path)
                             ;; error response structure is not standardized
                             (callback {:status "error"
                                        :message (str "could not retrieve " resolved-resource-path)}
                                       nil)
                             (.readFile fs
                                        resolved-resource-path
                                        "utf-8"
                                        (fn [err text]
                                          (resolve-content-and-return! text)))))))

                     ("git")
                     (fn [directive callback]
                       (let [resource-spec (sc/parse-link directive)
                             gra (-> resource-spec
                                     (:link)
                                     (git/parse-git-protocol-blob-path))]
                         (git/resolve-git-resource-address
                          gra
                          (fn [full-content]
                            (some->> (ci/resolve-content resource-spec full-content)
                                     (assoc resource-spec :text)
                                     (clj->js)
                                     (callback nil)))
                          (fn [_]
                            (some->> {:status "failed"}
                                     (clj->js)
                                     (callback nil))))))
                   
                     ("calibre" "zotero")
                     (fn [directive callback]
                       (let [resource-spec (sc/parse-link directive)]
                         (load-by-resource-resolver
                          resource-spec
                          callback)))

                     (fn [& _]
                       (callback nil {:message (str "failed to process directive "
                                                    directive)})))
                 
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
    }))

(defn start-server! []
  (let [app (express)
        JrpcServer (aget jsonrpc "server")
        ServerMiddleware (aget jsonrpc
                               "transports"
                               "server"
                               "middleware")]
    (doto app
      (.use (cors))
      (.use (js-invoke body-parser "json"))
      (.use (env/get :jsonrpc-endpoint)
            (aget (JrpcServer.
                   (ServerMiddleware.)
                   (clj->js $handler-mapping))
                  "transport"
                  "middleware"))
      (.listen $JSONRPC-PORT
               (fn []
                 (js/console.log
                  (str "starting rpc server on port "
                       $JSONRPC-PORT
                       "...")))))))

(defn -main []
  (start-server!))

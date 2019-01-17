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
   [xcl.core :as sc]
   [xcl.content-interop :as ci]
   [xcl.external :as ext]
   [xcl.pdfjslib-interop :as pdfjslib]
   [xcl.node-epub-interop :as epubi]
   [xcl.node-interop :as ni]
   [xcl.calibre-interop :as calibre]
   [xcl.zotero-interop :as zotero]))


(def $JSONRPC-PORT 5000)

(def $resource-resolver-loader-mapping
  (atom {:calibre-file
         (fn [spec callback]
           (calibre/load-epub
            (str
             "*"
             (:resource-resolver-path spec)
             "*.epub")
            spec
            (fn [text]
              (js/console.log
               (str "calibre epub: "
                    (count text)
                    " bytes\n\n"))
              (->> text
                   (clojure.string/trim)
                   (callback nil)))))

         :zotero-file
         (fn [spec callback]
           (zotero/load-pdf
            (str
             "*"
             (:resource-resolver-path spec)
             "*.pdf")
            spec
            (fn [page text]
              (js/console.log
               (str "zotero pdf:\n"
                    "page: " page "\n"
                    (count text)
                    " bytes\n\n"))
              (->> text
                   (clojure.string/trim)
                   (callback nil)))
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
                           (callback nil)))))))}))

(defn load-by-resource-resolver [spec callback]
  (when-let [loader (@$resource-resolver-loader-mapping
                     (:resource-resolver-method spec))]
    (loader spec callback)))

(let [xcl-file-resolver 
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
      
      resource-resolver-handler
      (fn [directive callback]
        (let [resource-spec (sc/parse-link directive)]
          (load-by-resource-resolver
           resource-spec
           callback)))]
  (def $handler-mapping
    {:file xcl-file-resolver
     :xcl xcl-file-resolver
     :calibre resource-resolver-handler
     :zotero resource-resolver-handler}))

(defn start-server! []
  (let [app (express)
        JrpcServer (aget jsonrpc "server")
        ServerMiddleware (aget jsonrpc
                               "transports"
                               "server"
                               "middleware")]
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
  (js/console.log
   (str "starting rpc server on port "
        $JSONRPC-PORT
        "..."))
  (start-server!))

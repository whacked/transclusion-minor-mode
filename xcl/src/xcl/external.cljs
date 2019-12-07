(ns xcl.external
  (:require ["js-yaml" :as yaml]
            ["jsonpath-plus" :as JSONPath]))

(def $ExternalLoaders (atom {}))

(defn register-loader! [extension loader]
  (js/console.warn "REGISTERING LOADER FOR" extension)
  (swap! $ExternalLoaders assoc extension loader))

(defn read-json [json-string]
  (js/JSON.parse json-string))

(defn read-yaml [yaml-string]
  (.safeLoad yaml yaml-string))

(defn read-jsonpath-content [js-object jsonpath]
  (js-invoke
   JSONPath
   "JSONPath"
   (clj->js {:json js-object
             :path jsonpath})))

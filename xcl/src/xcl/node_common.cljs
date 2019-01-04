(ns xcl.node-common
  (:require ["fs" :as fs]
            ["path" :as path]))

(defn path-exists? [p]
  (when p
    (.existsSync fs p)))

(defn path-join [& ps]
  (apply (aget path "join") ps))

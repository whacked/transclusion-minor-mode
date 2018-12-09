(ns xcl.external
  (:require))

(def $ExternalLoaders (atom {}))

(defn register-loader! [extension loader]
  (js/console.warn "REGISTERING LOADER FOR" extension)
  (swap! $ExternalLoaders assoc extension loader))

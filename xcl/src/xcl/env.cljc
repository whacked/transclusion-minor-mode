(ns xcl.env
  (:refer-clojure :exclude [get])
  (:require [shadow-env.core :as env]
            #?(:clj [clojure.data.json :as json])))

;; write a Clojure function that returns variables to expose to :clj and :cljs.
;; the function must accept one variable, the shadow-cljs build-state
;; (which will be `nil` initially, before compile starts)
#?(:clj
   (defn read-env [build-state]
     (let [config-data (-> (clojure.java.io/file
                            (System/getProperty "user.dir")
                            "config.json")
                           (slurp)
                           (json/read-str :key-fn keyword))
           out {:common {:user.dir (System/getProperty "user.dir")}
                :clj    {}
                :cljs   config-data}]
       out)))

;; define & link a new var to your reader function.
;; you must pass a fully qualified symbol here, so syntax-quote (`) is useful.
;; in this example I use `get` as the name, because we can call Clojure maps
;; as functions, I like to alias my env namespace as `env`, and (env/get :some-key)
;; is very readable.
(env/link get `read-env)

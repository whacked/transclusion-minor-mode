(ns xcl.static-loader
  (:require [clojure.java.io :as io]))

(defmacro slurp-file
  [fname]
  (clojure.string/replace
   (slurp
    (io/file
     (System/getProperty "user.dir")
     ".."
     fname))
   #"\r\n" "\n"))

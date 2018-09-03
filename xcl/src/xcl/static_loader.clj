(ns xcl.static-loader
  (:require [clojure.java.io :as io]))

(defmacro slurp-file
  [fname]
  (->> fname
       (io/file
        (System/getProperty "user.dir")
        "..")
       (slurp)))

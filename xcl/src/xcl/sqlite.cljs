(ns xcl.sqlite
  (:require ["sqlite3" :as sqlite3]
            ["yesql" :as yesql]
            ["path" :as path]
            [xcl.node-common :refer
             [path-exists? path-join]]))

(def $sql
  (yesql (path-join (.cwd js/process) "src")))

(defn filename-glob-to-query [filename]
  (-> (.parse path filename)
      (aget "name")
      (clojure.string/lower-case)
      (clojure.string/replace #"\*" "%")))

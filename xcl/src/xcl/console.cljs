(ns xcl.console)

(defn nocolor [& ss]
  (str (apply str ss) "\033[0m"))

(defn red [& ss]
  (apply nocolor "\033[31m" ss))
(defn log-red [s]
  (js/console.log (red s)))

(defn green [& ss]
  (apply nocolor "\033[32m" ss))
(defn log-green [s]
  (js/console.log (green s)))

(defn yellow [& ss]
  (apply nocolor "\033[33m" ss))
(defn log-yellow [s]
  (js/console.log (yellow s)))

(defn blue [& ss]
  (apply nocolor "\033[34m" ss))
(defn log-blue [s]
  (js/console.log (blue s)))

(defn magenta [& ss]
  (apply nocolor "\033[36m" ss))
(defn log-magenta [s]
  (js/console.log (magenta s)))

(defn cyan [& ss]
  (apply nocolor "\033[36m" ss))
(defn log-cyan [s]
  (js/console.log (cyan s)))

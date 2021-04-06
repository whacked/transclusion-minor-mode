(require 'parseedn)
(require 'ht)  ;; https://github.com/Wilfred/ht.el

;; TODO: need better way to sync + share server/client config

(defun xcl-load-edn (file-path)
  (parseedn-read-str
   (with-temp-buffer
     (insert-file-contents file-path)
     (buffer-string))))

(defvar xcl-repo-path nil "path to local xcl repo (currently used to load the server config)")
(defvar xcl-config nil "xcl config as an emacs hash-table")
(defvar xcl-server-rpc-endpoint "/rpc" "path in http[s]://hostname:port/<path>")

(unless (and xcl-repo-path xcl-config)
  (progn
    (setq xcl-repo-path (read-file-name "path to xcl repo: "))
    (setq
     xcl-config
     (ht-merge
      (xcl-load-edn (concat
                     (file-name-as-directory xcl-repo-path)
                     "default-config.edn"))
      (xcl-load-edn (concat
                     (file-name-as-directory xcl-repo-path)
                     "config.edn"))))))

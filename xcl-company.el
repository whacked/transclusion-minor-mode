(require 'cl-lib)
(require 'json-rpc)

(load-file "./xcl-config.el")

(defun xcl-query--search-text (token)
  ;; returns: { type: "array", items: { type: "string" } }
  ;; example json rpc text search request
  ;; (xcl-query--search-text "emacs")
  (json-rpc-request
   (json-rpc-connect
    (gethash :jsonrpc-host xcl-config)
    (gethash :jsonrpc-port xcl-config))
   xcl-server-rpc-endpoint
   "searchText"
   (list :text token)))

(defun company-xcl--make-candidate (file-path)
  (propertize (file-name-nondirectory file-path)
              'meta file-path))

(defun company-xcl--candidates (prefix)
  (let ((search-results
         (xcl-query--search-text prefix)))
    (let (res)
      (dolist (file-path
               (delete-dups (append search-results nil)))
        (push (company-xcl--make-candidate file-path) res))
      res)))

(defun company-xcl--annotation (candidate)
  ;; displayed to the right of the main entry in the popup
  (format " at %s" (get-text-property 0 'meta candidate)))

(defun company-xcl--meta (candidate)
  ;; displayed in minibuffer
  (let ((file-path (substring-no-properties candidate)))
    (format "%s at %s"
            candidate
            (get-text-property 0 'meta candidate))))

(defun company-xcl (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-xcl))
    (prefix (when (looking-back "\\[\\[\\..+")
              (file-name-base (company-grab-symbol))))
    (candidates (company-xcl--candidates arg))
    (annotation (company-xcl--annotation arg))
    (meta (company-xcl--meta arg))))

;; for testing
;; (local-set-key (kbd "M-n") 'company-xcl)
(define-key xcl-transclude-mode-map (kbd "M-n") 'company-xcl)

;;; xcl-transclude.el --- transclusion facility using overlays  -*- lexical-binding: t; -*-

;; Author: whacked
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (ov "1.0.6") (json-rpc "0.0.1"))
;; Keywords: convenience outlines transclusion
;; URL: https://github.com/whacked/transclusion-minor-mode

;;; Commentary:

;; unidirectional transclusion
;; the main funcion of interest is
;; xcl-transclude-toggle-overlay()
;; 
;; LIMITATIONS
;; - no logic that handles source file modifications outside of emacs

(require 'subr-x)
(require 'thingatpt)
(require 'cl)
(require 'cl-lib)
(require 'ov)
(require 'json-rpc)

(load-file "./xcl-config.el")
(load (concat
       default-directory
       ;; git submodule
       "ext--json-rpc-request/json-rpc-request"))

(setq XCL-TRANSCLUDE--SERVER-HOST
      ;; TODO: server-side defaults to "localhost"
      (gethash :jsonrpc-host xcl-config))

(setq XCL-TRANSCLUDE--SERVER-PORT
      ;; TODO: server-side defaults to 23120
      (gethash :jsonrpc-port xcl-config))

(setq xcl-transclude--overlay-color-base
      ;; TODO: server-side defaults to "papayawhip"
      ;;       but this is a client-side config
      (gethash :overlay-color-base xcl-config))

(setq xcl-transclude--overlay-color-modified
      ;; TODO: server-side defaults to "gold"
      ;;       but this is a client-side config
      (gethash :overlay-color-modified xcl-config))

(setq xcl-transclude--transclusion-directive-regexp
      "{{{transclude(.+?)}}}")

(setq xcl-transclude--active-source-file-hash
      (make-hash-table :test 'equal))


(defvar-local IPC-TERMINAL-CHARACTER "\x0c")
(defvar-local XCL-SERVER-NAME "xcl-server")
(defvar-local XCL-CONNECTION-BUFFER-NAME "*xcl-server*")
;; from node-ipc's default setup
(defvar-local IPC-SOCKET-PATH (concat "/tmp/app." XCL-SERVER-NAME))
(defvar XCL-SOCKET-CONNECTION nil)


(defun xcl-transclude--add-tracked-overlay (source-file-name target-file-name)
  (let ((current-list (gethash target-file-name
                               xcl-transclude--active-source-file-hash)))
    (puthash
     target-file-name
     (if current-list
         (cons source-file-name current-list)
       (list source-file-name))
     xcl-transclude--active-source-file-hash)))

(defun xcl-transclude--remove-tracked-overlay (source-file-name target-file-name)
  (let* ((current-list (gethash target-file-name
                                xcl-transclude--active-source-file-hash))
         (new-list (cl-remove source-file-name
                              current-list
                              :test 'equal
                              :count 1)))
    (puthash target-file-name new-list
             xcl-transclude--active-source-file-hash)
    new-list))

(defun org-macro-expression-at-point ()
  (interactive)
  (save-excursion
    (when (< (point) (1+ (buffer-size)))
      (let ((char-at-point (string (char-after (point)))))
        (cond ((string= char-at-point "{")
               (forward-char 3))
              ((string= char-at-point "}")
               (backward-char 2)))
        (let ((maybe-start-match (search-backward "{{{" (line-beginning-position) t)))
          (if (not maybe-start-match)
              (progn (message "no start match found")
                     nil)
            (let ((maybe-end-match (search-forward "}}}" (line-end-position) t)))
              (if (not maybe-end-match)
                  (progn (message "no end match found")
                         nil)
                (let ((macro-string
                       (string-trim
                        (buffer-substring-no-properties
                         (+ maybe-start-match 3)
                         (- maybe-end-match 3)))))
                  (list :directive macro-string
                        :beginning maybe-start-match
                        :end maybe-end-match))))))))))

(defun parse-transclusion-directive (directive)
  (let* ((re-pre "\s*transclude\s*(\s*"          )
         (re-post                      "\s*)\s*$"))
    (cond ((string-match
            (concat re-pre
                    "\\(\[^:\]+\s*\\)"
                    re-post)
            directive)
           ;; TODO revisit this naming scheme, or move this to a constant
           ;; "file1" is probably supposed to mean "emacs-native file"
           (list :protocol "file1"
                 :target (match-string 1 directive)))

          ((string-match
            (concat re-pre
                    "\\(\[^:\]+\s*::\s*.+?\\)"
                    re-post)
            directive)
           (list :protocol "file"
                 :target (match-string 1 directive)))

          ((string-match
            (concat re-pre
                    "\\(\[^:\]+\\)\s*:\s*\\(.+?\\)"
                    re-post)
            directive)
           (list :protocol (match-string 1 directive)
                 :target (match-string 2 directive))))))

(ert-deftest parse-transclusion-directive-test ()
  (let ((spec (parse-transclusion-directive
               "transclude(LICENSE::2,10)")))
    (should (string= (plist-get spec :protocol)
                     "file"))
    (should (string= (plist-get spec :target)
                     "LICENSE::2,10")))
  (let ((spec (parse-transclusion-directive
               "transclude(file:transcluding-org-elements.org)")))
    (should (string= (plist-get spec :protocol)
                     "file"))
    (should (string= (plist-get spec :target)
                     "transcluding-org-elements.org")))
  (let ((spec (parse-transclusion-directive
               "transclude(file:./transcluding-org-elements.org::2-10)")))
    (should (string= (plist-get spec :protocol)
                     "file"))
    (should (string= (plist-get spec :target)
                     "./transcluding-org-elements.org::2-10")))
  (let ((spec (parse-transclusion-directive
               "transclude(xcl:./public/tracemonkey.pdf?p=3&s=Monkey observes that...so TraceMonkey attempts)")))
    (should (string= (plist-get spec :protocol)
                     "xcl"))
    (should (string= (plist-get spec :target)
                     "./public/tracemonkey.pdf?p=3&s=Monkey observes that...so TraceMonkey attempts")))
  (let ((spec (parse-transclusion-directive
               "transclude(calibre:quick start?s=The real advantage...on your computer.)")))
    (should (string= (plist-get spec :protocol)
                     "calibre"))
    (should (string= (plist-get spec :target)
                     "quick start?s=The real advantage...on your computer.")))
  (let ((spec (parse-transclusion-directive
               "transclude(dummy:something)")))
    (should (string= (plist-get spec :protocol)
                     "dummy"))
    (should (string= (plist-get spec :target)
                     "something"))))

(defun transclusion-directive-at-point ()
  (interactive)
  (let ((maybe-transclusion-directive
         (org-macro-expression-at-point)))
    ;; TODO refactor this part to function
    (when maybe-transclusion-directive
      (let ((parsed-expression
             (parse-transclusion-directive
              (plist-get maybe-transclusion-directive :directive))))
        (append parsed-expression
                maybe-transclusion-directive)))))

(when nil
  ;; unused
  
  (defun xcl-transclude--is-all-keys-present? (required-keys plist)
    (let ((ok? t))
      (loop for key in required-keys
            do (let ()
                 (when (not
                        (plist-get plist key))
                   (warn (format
                          "required key %s not found in spec"
                          key))
                   (setq ok? nil))))
      (when ok?
        plist)))

  (defun xcl-transclude--validate-resource-spec (spec)
    (xcl-transclude--is-all-keys-present?
     (list :directive :content)
     spec))

  (defun xcl-transclude--spec-base-object (&rest plist-data)
    (funcall 'xcl-transclude--validate-resource-spec plist-data)))


(defun xcl-get-ipc-connection ()
  (when (or (null XCL-SOCKET-CONNECTION)
            (not (process-live-p XCL-SOCKET-CONNECTION)))
    (setq
     XCL-SOCKET-CONNECTION
     (make-network-process
      :name XCL-SERVER-NAME
      :buffer XCL-CONNECTION-BUFFER-NAME
      :remote IPC-SOCKET-PATH)))
  XCL-SOCKET-CONNECTION)

(defun xcl-make-socket-jsonprc-request (method params)
  ;; (xcl-make-socket-jsonprc-request "echo" (list :hello "world"))
  (let ((proc (xcl-get-ipc-connection)))
    (let ((response
           (with-current-buffer (process-buffer proc)
             (erase-buffer)
             (process-send-string
              proc
              (concat
               (json-encode
                (list
                 :type "jsonrpc"
                 :data (list
                        :jsonrpc "2.0"
                        :id 1 ;; don't care
                        :method method
                        :params params)))
               IPC-TERMINAL-CHARACTER))
             (accept-process-output proc)
             (message (buffer-string)))))
      
      ;; TODO: figure out a better way to manage the process
      ;; (delete-process proc)
      
      ;; matches configuration in https://github.com/skeeto/elisp-json-rpc/blob/master/json-rpc.el#L154
      (let* ((json-object-type 'plist)
             (json-array-type 'keyword))
        (json-read-from-string response)))))

(defun xcl-transclude--json-rpc-request (rpc-command spec)
  (let* ((protocol (plist-get spec :protocol))
         (target-path (plist-get spec :target)))
    ;; returns shape like
    ;; (list
    ;;  :link "file:/path/to/target.org"
    ;;  :post-processors nil
    ;;  :protocol "file"
    ;;  :resource-resolver-path "/path/to/target.org"
    ;;  :resource-resolver-method "exact-name" :content-resolvers
    ;;  [(:type "whole-file")]
    ;;  :text "* example\n\nHello, 世界")
    
    (json-rpc-request
     (json-rpc-connect XCL-TRANSCLUDE--SERVER-HOST XCL-TRANSCLUDE--SERVER-PORT)
     xcl-server-rpc-endpoint
     rpc-command
     (list :protocol protocol
           :directive (concat
                       protocol ":"
                       (if (string= protocol "file")
                           ;; convert relpath to abspath for the server
                           ;; WARNING: POSIX ONLY
                           (if (string-prefix-p "/" target-path)
                               target-path
                             (concat default-directory
                                     target-path))
                         ;; other protocols stay as-is
                         target-path))))))

(defun xcl-transclude--slurp-file (file-path)
  ;; convenience method
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun xcl-transclude--retrieve-content-from-spec (spec)
  ;; (xcl-transclude--retrieve-content-from-spec
  ;;  (parse-transclusion-directive
  ;;   "transclude(...)"))
  ;; 
  ;; where ... is e.g.:
  ;; "xcl:./public/tracemonkey.pdf?p=3&s=Monkey observes that...so TraceMonkey attempts"
  ;; "xcl:./public/alice.epub?p=2&s=Would you tell me, please...walk long enough"
  ;; "calibre:quick start?s=The real advantage...on your computer."
  ;; "zotero:just-in-time?s=Monkey observes...TraceMonkey attempts"
  ;; 
  ;; returns plist e.g.
  ;; (:text "foo bar baz" ...)
  ;; where ... is the rest of the parsed directive
  
  (message "[XCL] retrieving for spec: %s" spec)
  (let ((xcl-protocol (plist-get spec :protocol)))
    (cond ((string= "file1" xcl-protocol)
           ;; (shortcut) handle natively
           (let ((file-path (plist-get spec :target)))
             (list
              :link file-path
              :post-processors nil
              :protocol "file1"
              :resource-resolver-path file-path
              :resource-resolver-method "exact-name"
              :content-resolvers [(:type "whole-file")]
              :text (xcl-transclude--slurp-file file-path))))
          
          (t
           ;; (fallback)
           (xcl-transclude--json-rpc-request "get-text" spec)))))

(defun xcl-transclude--open-file-from-spec (spec)
  (message "[XCL] opening for spec: %s" spec)
  (xcl-transclude--json-rpc-request "open" spec))

(defun xcl-transclude--create-overlay!
    (ov-point-beg parsed-resource-spec)
  (let ((ov nil)
        (content (plist-get parsed-resource-spec :text))
        (modified-p (buffer-modified-p)))
    (save-excursion
      (goto-char ov-point-beg)
      (setq ov (ov-read-only
                (ov-insert content)))
      (overlay-put ov 'evaporate t)
      
      ;; set the color of the text
      (overlay-put ov 'face
                   (cons 'background-color xcl-transclude--overlay-color-base))
      
      ;; add the modification hooks (after we have inserted)
      (overlay-put ov 'modification-hooks
                   '(xcl-transclude--overlay-mark-as-modified))

      (overlay-put ov 'is-xcl-transclude-overlay t)
      (overlay-put ov 'directive
                   (plist-get parsed-resource-spec :directive))
      
      ;; set buffer modified to nil if it was not already modified
      (set-buffer-modified-p modified-p)
      ;; force overlay to be not modified
      (xcl-transclude--overlay-set-modified-status ov nil)
      
      (ov-read-only
       ov
       t ;; insert-in-front
       t ;; insert-behind
       )

      (let* ((resource-path (plist-get parsed-resource-spec
                                       :resource-resolver-path))
             (transcluded-file-path (file-truename resource-path))
             (maybe-file-buffer (get-file-buffer resource-path)))
        
        (overlay-put ov 'transcluded-file-path transcluded-file-path)
        
        (xcl-transclude--add-tracked-overlay
         (buffer-file-name)
         transcluded-file-path)
        
        (when maybe-file-buffer
          (xcl-transclude--add-save-hook-to-buffer
           maybe-file-buffer)))
      ov)))

(defun xcl-transclude--trigger-update-overlays ()
  (let* ((transcluded-file-path (buffer-file-name))
         (source-buffer-list
          (gethash (buffer-file-name)
                   xcl-transclude--active-source-file-hash)))
    (dolist (source-buffer-name (delete-dups source-buffer-list))
      (let ((maybe-source-buffer (get-file-buffer source-buffer-name)))
        (if maybe-source-buffer
            (with-current-buffer
                maybe-source-buffer
              (dolist (ov-to-update
                       (reverse
                        (ov-in 'transcluded-file-path transcluded-file-path)))
                (xcl-transclude--sync-overlay!
                 ov-to-update)))
          ;; source buffer no longer available, remove it
          (setq
           xcl-transclude--active-source-file-hash
           (puthash
            (buffer-file-name)
            (cl-delete
             source-buffer-name
             source-buffer-list)
            xcl-transclude--active-source-file-hash)))))))

(defun xcl-transclude--close-overlay! (&optional pos)
  (interactive (list (point)))
  (let ((ov-list (overlays-at (or pos (point)))))
    (while ov-list
      (let ((ov (car ov-list))
            (current-modified-p (buffer-modified-p)))
        (if (and ov-list (overlay-get ov 'is-xcl-transclude-overlay))
            (let ((transcluded-file-path (overlay-get ov 'transcluded-file-path)))
              (if (or (null (overlay-get ov 'is-modified))
                      (yes-or-no-p "source file for this overlay was modified. discard changes? "))
                  (let ((del-start (overlay-start ov))
                        (del-end (overlay-end ov)))
                    
                    (remove-overlays del-start del-end
                                     'transcluded-file-path
                                     transcluded-file-path)
                    
                    (xcl-transclude--remove-tracked-overlay
                     (buffer-file-name)
                     transcluded-file-path)
                    
                    (kill-region del-start del-end)
                    (set-buffer-modified-p current-modified-p))))))
      (setq ov-list (cdr ov-list)))))

;;;###autoload
(defun xcl-transclude-toggle-overlay (&optional xcl-transclude-plist)
  (interactive)
  ;; first check are we on an overlay?
  (let ((maybe-overlay (xcl-transclude--get-overlay-at-point)))
    (if maybe-overlay
        (xcl-transclude--close-overlay!)
      ;; then check if we are followed by an overlay?
      (let ((maybe-transclusion-directive
             (transclusion-directive-at-point)))
        (cond (maybe-transclusion-directive
               ;; check if directive is followed by an overlay
               (save-excursion
                 (goto-char
                  (+ 1 (plist-get maybe-transclusion-directive :end)))
                 (xcl-transclude-toggle-overlay maybe-transclusion-directive)))
              
              (xcl-transclude-plist
               (let* ((directive (plist-get
                                  xcl-transclude-plist
                                  :directive))
                      (ret-parsed-spec (xcl-transclude--retrieve-content-from-spec
                                        xcl-transclude-plist))
                      (overlay (xcl-transclude--create-overlay! (point) ret-parsed-spec)))
                 
                 ;; parsed looks like:
                 ;; (list :link "file:/path/to/transclusion-minor-mode/LICENSE::2,10"
                 ;;       :protocol "file"
                 ;;       :resource-resolver-path "/path/to/transclusion-minor-mode/LICENSE"
                 ;;       :resource-resolver-method "exact-name"
                 ;;       :content-resolvers [(:bound (:beg 2 :end 10) :type char-range)]
                 ;;       :text "U GENERA")
                 ret-parsed-spec))
              (t
               (message "no overlay and no transclusion directive")))))))

(defun xcl-transclude--enable-all-overlays! ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while
        (re-search-forward
         xcl-transclude--transclusion-directive-regexp
         nil t)
      (xcl-transclude-toggle-overlay))))

(defun xcl-transclude--disable-all-overlays! ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((overlays (ov-all)))
      (dolist (ov overlays)
        (when (overlay-get ov 'is-xcl-transclude-overlay)
          ;; NOTE: this seems to cause the buffer modified flag to change
          (delete-region (overlay-start ov)
                         (overlay-end ov))
          ;; this may be redundant due to 'evaporate
          (delete-overlay ov))))))

(defun xcl-transclude--sync-overlay! (overlay)
  (message "[XCL] syncing overlay: %s" overlay)
  (save-excursion
    (goto-char (ov-beg overlay))
    (xcl-transclude--close-overlay!)
    (search-backward "{{{")
    (xcl-transclude-toggle-overlay)))

(defun xcl-transclude--sync-overlays! ()
  (interactive)
  (dolist (ov-to-update (reverse (ov-in 'is-xcl-transclude-overlay t)))
    (xcl-transclude--sync-overlay! ov-to-update)))

(defun xcl-transclude--add-save-hook-to-buffer (target-buffer)
  (let ((maybe-file-buffer
         (if (eq 'string (type-of target-buffer))
             (get-file-buffer target-buffer)
           target-buffer)))
    (when maybe-file-buffer
      (message "[XCL] adding hook to: %s" maybe-file-buffer)
      (with-current-buffer maybe-file-buffer
        (add-hook
         'after-save-hook
         'xcl-transclude--trigger-update-overlays
         nil
         t ;; buffer local
         )))))

(defun xcl-transclude--attempt-visit-transclusion-directive (parsed-directive)
  (cond ((and parsed-directive
              (string= "file" (plist-get parsed-directive :protocol)))
         
         (let* ((target (plist-get parsed-directive :target))
		(maybe-split (split-string target "::"))
		(path-only (car maybe-split))
		(full-file-name
                 (expand-file-name
                  ;; need to post-process potential line number ranges
                  ;; for org-open-link-from-string
                  (if (= 1 (length maybe-split))
		      ;; no intra-locator
		      path-only
                    (let ((intra-locator
                           (cadr maybe-split)))
		      (cond ((string-prefix-p "*" intra-locator)
                             ;; e.g. file:my-file.org::*myheading
                             target)

                            ((string-match
			      "\\([[:digit:]]\\)+-.+"
			      intra-locator)
                             (concat
			      path-only "::"
			      (match-string 1 intra-locator)))

                            ((string-match-p
			      "[[:digit:]]+"
			      intra-locator)
                             (concat path-only "::" intra-locator))
                            
                            (t path-only)))))))
           (org-open-link-from-string
            (format "[[%s]]" full-file-name))
	   (message "TO OPEN: %s"
		    parsed-directive)
           (xcl-transclude--add-save-hook-to-buffer
            path-only)))
        
        (t
         (xcl-transclude--open-file-from-spec parsed-directive))))

;; override org's org-edit-special()
(defun xcl-transclude--transclusion-org-edit-special-advice (&optional ARG PRED) 
  (let ((maybe-overlay (xcl-transclude--get-overlay-at-point))
        (maybe-transclusion-directive
         (transclusion-directive-at-point)))
    
    (cond (maybe-overlay
           (let* ((source-directive
                   (overlay-get maybe-overlay 'directive))
                  (parsed-directive
                   (parse-transclusion-directive
                    source-directive)))
             (xcl-transclude--attempt-visit-transclusion-directive
              parsed-directive)
             t))
          
          (maybe-transclusion-directive
           (xcl-transclude--attempt-visit-transclusion-directive
            maybe-transclusion-directive)
           t))))

(advice-add 'org-edit-special :before-until
            'xcl-transclude--transclusion-org-edit-special-advice)

;; ref http://emacs.stackexchange.com/a/358
(defvar xcl-transclude-mode-map (make-sparse-keymap)
  "Keymap while xcl-transclude-mode is active.")

;; ref http://emacs.stackexchange.com/questions/352/how-to-override-major-mode-bindings
;; ref http://nullprogram.com/blog/2013/02/06/
;;;###autoload
(define-minor-mode xcl-transclude-mode
  "A minor mode so that my key settings override annoying major modes."
  nil
  :lighter "xcl-transclude-mode"
  xcl-transclude-mode-map)

;; Source: http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'xcl-transclude-mode))
      (let ((mykeys (assq 'xcl-transclude-mode minor-mode-map-alist)))
        (assq-delete-all 'xcl-transclude-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

(define-globalized-minor-mode global-xcl-transclude-mode xcl-transclude-mode turn-on-xcl-transclude-mode)

(defun xcl-transclude--get-overlay-at-point ()
  (interactive)
  (let ((ov-list (overlays-at (point)))
        rtn)
    (while ov-list
      (let ((ov (car ov-list)))
        (if (overlay-get ov 'is-xcl-transclude-overlay)
            (setq rtn ov)))
      (setq ov-list (cdr ov-list)))
    rtn))

;; to deprecate
(defun xcl-transclude--overlay-set-modified-status (ov is-modified?)
  (if is-modified?
      (progn (overlay-put ov 'is-modified t)
             (overlay-put ov 'face
                          (cons 'background-color xcl-transclude--overlay-color-modified)))
    (progn (overlay-put ov 'is-modified nil)
           (overlay-put ov 'face
                        (cons 'background-color xcl-transclude--overlay-color-base)))))

;; to deprecate
(defun xcl-transclude--overlay-mark-as-modified
    ;; see https://github.com/gregdetre/emacs-freex/blob/master/freex-embed.el#L343
    (overlay is-post start end &optional replaced-length)
  "This is the function that gets called by the overlay's
  modified hook. Must have these arguments."
  (let ((current-modified-p (buffer-modified-p)))
    (when is-post
      (xcl-transclude--overlay-set-modified-status overlay t)
      (set-buffer-modified-p current-modified-p))))

(define-key xcl-transclude-mode-map (kbd "C-c E") 'xcl-transclude-toggle-overlay)

(provide 'xcl-transclude)
;;; xcl-transclude.el ends here

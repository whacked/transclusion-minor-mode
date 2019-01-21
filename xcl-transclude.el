;;; xcl-transclude.el --- transclusion facility using overlays  -*- lexical-binding: t; -*-

;; Author: whacked
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (ov "1.0.6") (json-rpc "0.0.1"))
;; Keywords: convenience outlines transclusion
;; URL: https://github.com/whacked/transclusion-minor-mode

;;; Commentary:

;; unidirectional transclusion

(require 'subr-x)
(require 'thingatpt)
(require 'cl)
(require 'cl-lib)
(require 'ov)
(require 'json-rpc)
(load (concat
       default-directory
       ;; git submodule
       "ext--json-rpc-request/json-rpc-request"))

(let* ((config-file-path "xcl/config.json")
       (config (if (file-exists-p config-file-path)
                   (json-read-file config-file-path)
                 nil)))

  (setq XCL-TRANSCLUDE--SERVER-HOST
        (or (cdr (assoc 'jsonrpc-host config))
            "localhost"))
  
  (setq XCL-TRANSCLUDE--SERVER-PORT
        (or (cdr (assoc 'jsonrpc-port config))
            23120))

  (setq xcl-transclude--overlay-color-base
        (or (cdr (assoc 'overlay-color-base config))
            "papayawhip"))
  
  (setq xcl-transclude--overlay-color-modified
        (or (cdr (assoc 'overlay-color-modified config))
            "gold")))

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
              (message "no start match found")
            (let ((maybe-end-match (search-forward "}}}" (line-end-position) t)))
              (if (not maybe-end-match)
                  (message "no end match found")
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




;; ISSUES & LIMITATIONS
;; - modifying the overlay sets (buffer-modified-p) of the master
;;   buffer even if the master buffer was not modified; currently
;;   just a minor annoyance
;; - does not have logic that handles source file modifications
;;   outside of emacs


;; TODO: give more unified name + cleanup
;; moved in from old dot
(defun myembed-split-window ()
  "prototype embed buffer fn for editing #+INCLUDE files 'in-place'"
  (interactive)
  (when (save-excursion
        (beginning-of-line 1)
        (looking-at (concat
                     ;; take regexp from org.el:org-edit-special
                     "\\(?:#\\+\\(?:setupfile\\|include\\):?[ \t]+\"?\\|[ \t]*<include\\>.*?file=\"\\)\\([^\"\n>]+\\)"
                     ;; sloppily match :lines ### portion
                     ".+\\(?::lines\\)[ \t]+\\([0-9]*\\)[-~ ]\\([0-9]*\\)")))
    (let* ((file-to-visit (org-trim (match-string 1)))
           (line-to-visit (string-to-number (match-string 2)))
           (new-window-height (max
                               (- (string-to-number (match-string 3))
                                  line-to-visit)
                               5))
           (cur-line-num (line-number-at-pos))
           (top-line-num (save-excursion
                           (move-to-window-line-top-bottom 0)
                           (line-number-at-pos)))
          (cur-window-height (window-height)))
      
      (split-window nil (+ 4 (- cur-line-num top-line-num)))
      (recenter-top-bottom -1)
      (other-window 1)
      (split-window nil new-window-height)
      (find-file file-to-visit)
      (other-window 1)
      (goto-line (+ cur-line-num 1))
      (recenter-top-bottom 0)
      (other-window -1)
      (goto-line line-to-visit)
      (recenter-top-bottom))))
;; never actually used this...
;; (global-set-key "\C-cE" 'myembed-split-window)

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

;; Turn off the minor mode in the minibuffer
(add-hook 'minibuffer-setup-hook 'turn-off-xcl-transclude-mode)

;;;###autoload
(defun turn-on-xcl-transclude-mode ()
  "Turns on xcl-transclude-mode."
  (interactive)
  (xcl-transclude-mode t))

;;;###autoload
(defun turn-off-xcl-transclude-mode ()
  "Turns off xcl-transclude-mode."
  (interactive)
  (xcl-transclude-mode -1))

;;;###autoload
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


(provide 'xcl-transclude)
;;; xcl-transclude.el ends here

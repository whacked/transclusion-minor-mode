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
  "Keymap while transclude-mode is active.")

;; ref http://emacs.stackexchange.com/questions/352/how-to-override-major-mode-bindings
;; ref http://nullprogram.com/blog/2013/02/06/
;;;###autoload
(define-minor-mode transclude-mode
  "A minor mode so that my key settings override annoying major modes."
  nil
  :lighter "transclude-mode"
  xcl-transclude-mode-map)

;; Source: http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'transclude-mode))
      (let ((mykeys (assq 'transclude-mode minor-mode-map-alist)))
        (assq-delete-all 'transclude-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

;;;###autoload
(define-globalized-minor-mode global-transclude-mode transclude-mode turn-on-transclude-mode)

;; Turn off the minor mode in the minibuffer
(add-hook 'minibuffer-setup-hook 'turn-off-transclude-mode)

;;;###autoload
(defun turn-on-transclude-mode ()
  "Turns on my-mode."
  (interactive)
  (transclude-mode t))

;;;###autoload
(defun turn-off-transclude-mode ()
  "Turns off my-mode."
  (interactive)
  (transclude-mode -1))

(defun sweep-and-refresh-matching-overlay (source-filepath)
  (let ((check-buffer-list (buffer-list)))
    (while check-buffer-list
      (if (and (buffer-file-name (car check-buffer-list))
               (string= source-filepath
                        (expand-file-name
                         (buffer-file-name (car check-buffer-list)))))
          (let ((now-buf (current-buffer)))
            (set-buffer (car check-buffer-list))
            (revert-buffer nil t)))
      (setq check-buffer-list (cdr check-buffer-list)))))

(setq freex-active-source-file-list nil)
(make-variable-buffer-local 'freex-active-source-file-list)
;; force-set
;; (setq-local freex-active-source-file-list (list "/path/to/file.org"))

(setq freex-color-base "#777777")
(setq freex-color-modified "#993333")

(defun freex-get-overlay-at-point ()
  (interactive)
  (let ((ov-list (overlays-at (point)))
        rtn)
    (while ov-list
      (let ((ov (car ov-list)))
        (if (overlay-get ov 'is-freex-embed)
            (setq rtn ov)))
      (setq ov-list (cdr ov-list)))
    rtn))

(defun freex-close-overlay (pos)
  (interactive (list (point)))
  (let ((ov-list (overlays-at pos)))
    (while ov-list
      (let ((ov (car ov-list))
            (current-modified-p (buffer-modified-p)))
        (if (and ov-list (overlay-get ov 'is-freex-embed))
            (let ((source-filepath (overlay-get ov 'full-filename)))
              (if (or (null (overlay-get ov 'is-modified))
                      (yes-or-no-p "source file for this overlay was modified. discard changes? "))
                  (let ((del-start (overlay-start ov))
                        (del-end (overlay-end ov)))
                    (remove-overlays 1 (buffer-size) 'full-filename source-filepath)
                    (setq-local freex-active-source-file-list
                                (delete source-filepath
                                        (buffer-local-value 'freex-active-source-file-list (current-buffer))))
                    (kill-region del-start del-end)
                    (set-buffer-modified-p current-modified-p))))))
      (setq ov-list (cdr ov-list)))))


(defun freex-overlay-set-modified-status (ov is-modified?)
  (if is-modified?
      (progn (overlay-put ov 'is-modified t)
             (overlay-put ov 'face
                          (cons 'background-color freex-color-modified)))
    (progn (overlay-put ov 'is-modified nil)
           (overlay-put ov 'face
                        (cons 'background-color freex-color-base)))))

(defun get-text-between-2-line-index (start-line until-line)
  "until IS included! this is for the human-friendly specification
   by 'lines A to B' or L20~24 etc., thus it is inclusive"
  (let (start-pos end-pos)
    (save-excursion
      (setq start-pos
            (if (null start-line)
                0
              (progn
                (goto-char (point-min))
                (forward-line (1- start-line))
                (point))))
      (setq end-pos
            (if (null until-line)
                (buffer-size)
              (progn
                (goto-char (point-min))
                (forward-line until-line)
                (point)))))
    (buffer-substring-no-properties start-pos end-pos)))

;; note: we assume there will only be 1 overlay at a time!
;; so we only check `car`
(defun check-overlay-and-save (&optional whatever)
  (interactive)
  ;; (say "保存しましょう4")
  
  (let ((overlays (overlays-at (point)))
        freex-ov)
    ;; fish out the freex overlay if any
    (while overlays
      (let ((ov (car overlays)))
        (if (overlay-get ov 'is-freex-embed)
            (setq freex-ov ov)))
      (setq overlays (cdr overlays)))
    (if (and freex-ov
             (overlay-get freex-ov 'is-freex-embed)
             (overlay-get freex-ov 'is-modified))
        (let ((overlay-text (buffer-substring (overlay-start freex-ov)
                                              (overlay-end freex-ov)))
              (source-filepath (overlay-get freex-ov 'full-filename))
              (current-modified-p (buffer-modified-p)))
          (with-temp-buffer
            (insert overlay-text)
            (write-file source-filepath nil))
          
          ;; TODO
          ;; upon opening a new overlay
          ;; loop in all current open files
          ;; if file is same as overlay source file
          ;; add save hook so that
          ;; [X] on save of that source file, overlay is updated
          ;;     --> should be handled by freex-update-active-overlay-list()
          ;; [X] on save of overlay, source file is updated
          (sweep-and-refresh-matching-overlay source-filepath)
          
          ;; force overlay to be not modified
          (freex-overlay-set-modified-status freex-ov nil)
          (set-buffer-modified-p current-modified-p)
          (message (format "saved %s" (overlay-get freex-ov 'full-filename))))
      (let ((current-filepath (expand-file-name (buffer-file-name (current-buffer)))))
        ;; "normal" save
        ;; go through all current overlays, exclude all freex overlays,
        ;; and write out the resulting text (sans freex overlay texts)
        (let ((cur-buf (current-buffer))
              (max-idx (point-max)))
          (let ((ov-list (overlays-in 0 (buffer-size)))
                exclude-list)
            ;; compile list of indexes from freex overlays
            (while ov-list
              (let ((ov (car ov-list)))
                (if (overlay-get ov 'is-freex-embed)
                    (let ((ov-start (overlay-start ov))
                          (ov-end   (overlay-end ov)))
                      (if (< ov-start ov-end)
                          (setq exclude-list (cons (list ov-start ov-end)
                                                   exclude-list))))))
              (setq ov-list (cdr ov-list)))
            (message (format "***** exclude list %s\n" exclude-list))
            
            ;; loop through sorted idx list, and only insert
            ;; text that is not within them
            (let ((start-idx 1)
                  (sorted-idx-list (sort exclude-list #'(lambda (a b) (< (first a) (first b))))))
              
              (with-temp-buffer
                (while sorted-idx-list
                  (let ((idx-pair (car sorted-idx-list)))
                    (insert (with-current-buffer cur-buf
                              (buffer-substring start-idx (first idx-pair))))
                    (setq start-idx (second idx-pair))
                    (setq sorted-idx-list (cdr sorted-idx-list))))
                (if (< start-idx max-idx)
                    (insert (with-current-buffer cur-buf
                              (buffer-substring start-idx max-idx))))
                (write-file current-filepath)))))

        ;; XXX WARNING this is potentially dangerous if there are
        ;; concurrent writes or multiple windows of the same file
        ;; open. i have not thought through this.
        (set-buffer-modified-p nil)
        (clear-visited-file-modtime)
        
        )
      ))
  )

(defun overlay-mark-as-modified
    (overlay is-post start end &optional replaced-length)
  "This is the function that gets called by the overlay's
  modified hook. Must have these arguments."
  (let ((current-modified-p (buffer-modified-p)))
    (when is-post
      (freex-overlay-set-modified-status overlay t)
      (set-buffer-modified-p current-modified-p))))

;; TODO
;; [X] make line-start and line-end work (prelim)
(defun freex-create-overlay
    (beg source-filepath line-start line-end)
  (let ((properties (list 'full-filename source-filepath
                          'line-start 1
                          'line-end nil)))
    "simplified from freex-embed-create-overlay, but removed
     insert-funct, save-funct.

     create an overlay embed at point `beg` using contents
     of `source-filepath`
     "
    (let ((ov nil)
          ;; pre-read the source text, instead of using e.g.
          ;; (insert-file), because without knowing the
          ;; length of the text, we either resort to
          ;; creating a rear-advance overlay by calling
          ;; (make-overlay beg beg nil nil t) -- which is
          ;; what freex did -- else we get strange overlay
          ;; growing/shrinking effects when editing around
          ;; the edges
          (source-text (with-temp-buffer
                         (insert-file-contents source-filepath)
                         (let ((pt-start (progn
                                           (goto-line (or (null line-start) 1))
                                           (point)))
                               (pt-end   (progn
                                           (if (null line-end)
                                               (end-of-buffer)
                                             (goto-line (1+ line-end)))
                                           (point))))
                           (buffer-substring pt-start pt-end))))
          (modified-p (buffer-modified-p)))
      
      ;; add any optional properties to it, unless the
      ;; PROPERTIES list has an even number of items
      (unless (equal (mod (length properties) 2) 0)
        (error "Properties list must be even in length"))

      (save-excursion
        ;; make the overlay consist of a single newline, to
        ;; begin with
        (goto-char beg)

        (insert source-text)
        
        ;; start end
        ;; buffer (current-buffer)
        ;; front-advance nil
        ;; rear-advance nil
        (setq ov (make-overlay beg (+ beg (length source-text))))
        ;; add the default properties
        (overlay-put ov 'is-freex-embed t)
        (overlay-put ov 'evaporate nil)
        (overlay-put ov 'rear-nonsticky t)
        ;; set the color of the text
        (overlay-put ov 'face
                     (cons 'background-color
                           freex-color-base))
        (while properties
          (overlay-put ov (pop properties) (pop properties)))

        ;; add the modification hooks (after we have inserted)
        (overlay-put ov 'modification-hooks
                     '(overlay-mark-as-modified))
        ;; set buffer modified to nil if it was not already modified
        (set-buffer-modified-p modified-p)
        ;; force overlay to be not modified
        (freex-overlay-set-modified-status ov nil)

        (setq-local freex-active-source-file-list
                    (cons
                     source-filepath
                     (buffer-local-value 'freex-active-source-file-list (current-buffer))
                     ))
        ov))))

(defun nonzero-or-nil (s)
  (let ((num (string-to-number s)))
    (cond ((= 0 num)
           nil)
          (t num))))

(define-key xcl-transclude-mode-map (kbd "C-x C-s") 'check-overlay-and-save)

(provide 'xcl-transclude)
;;; xcl-transclude.el ends here

;; ISSUES & LIMITATIONS
;; - modifying the overlay sets (buffer-modified-p) of the master
;;   buffer even if the master buffer was not modified; currently
;;   just a minor annoyance
;; - does not have logic that handles source file modifications
;;   outside of emacs

;; XXX debug
;; (defun say (what-to-say) (shell-command (format "say %s" what-to-say)))

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
(global-set-key "\C-cE" 'myembed)

;; ref http://emacs.stackexchange.com/a/358
(defvar transclude-mode-map (make-sparse-keymap)
  "Keymap while transclude-mode is active.")

;;;###autoload
(define-minor-mode transclude-mode
  "A minor mode so that my key settings override annoying major modes."
  nil
  :lighter "transclude-mode"
  transclude-mode-map)

;; Source: http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'transclude-mode))
      (let ((mykeys (assq 'transclude-mode minor-mode-map-alist)))
        (assq-delete-all 'transclude-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)


;; FIXME change turn-on / turn-off naming to easier toggle
;;;###autoload
(defun turn-on-transclude-mode ()
  "Turns on transclude-mode."
  (interactive)
  (transclude-mode t))

;;;###autoload
(defun turn-off-transclude-mode ()
  "Turns off transclude-mode."
  (interactive)
  (transclude-mode -1))

;;;###autoload
(define-globalized-minor-mode global-transclude-mode transclude-mode turn-on-transclude-mode)

;; Turn off the minor mode in the minibuffer
(add-hook 'minibuffer-setup-hook 'turn-off-transclude-mode)


(defun sweep-and-refresh-matching-overlay (source-filepath)
  (let ((check-buffer-list (buffer-list)))
    (while check-buffer-list
      (if (string= source-filepath
                   (buffer-file-name (car check-buffer-list)))
          (let ((now-buf (current-buffer)))
            (switch-to-buffer (car check-buffer-list))
            (revert-buffer nil t)
            (switch-to-buffer now-buf)))
      (setq check-buffer-list (cdr check-buffer-list)))))

(setq freex-active-source-file-list nil)
(make-variable-buffer-local 'freex-active-source-file-list)
;; force-set
;; (setq-local freex-active-source-file-list (list "/path/to/file.org"))

(setq freex-color-base "#000000")
(setq freex-color-modified "#331111")

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

(defun freex-update-active-overlay-list ()
  ;; loop through the opened buffer list;
  ;; for each opened buffer
  ;; look at the buffer local `freex-active-source-file-list`
  ;; if it is non-nil and
  ;; if it contains the filepath of the buffer being saved,
  ;; refresh the overlays in that buffer
  ;; (say "保存 1")
  (let* ((src-buf (current-buffer))
         (source-filepath (buffer-file-name src-buf)))
    (let ((check-buffer-list (buffer-list)))
      (while check-buffer-list
        (let ((tgt-buf (car check-buffer-list)))
          (if (and (buffer-local-value 'freex-active-source-file-list tgt-buf)
                   (member source-filepath (buffer-local-value 'freex-active-source-file-list tgt-buf)))
              (progn
                (message (format "->>> MATCH ->>> %s FROM %s\n"
                                 (buffer-name tgt-buf)
                                 source-filepath))
                
                (switch-to-buffer tgt-buf)
                (let ((ov-list (overlays-in 0 (buffer-size)))
                      target-ov)
                  (while ov-list
                    (let ((ov (car ov-list)))
                      (if (and (overlay-get ov 'is-freex-embed)
                               (overlay-get ov 'full-filename))
                          (setq target-ov ov)))
                    (setq ov-list (cdr ov-list)))
                  (if target-ov
                      (let* ((line-start (overlay-get target-ov 'line-start))
                             (line-end   (overlay-get target-ov 'line-end))
                             (pos-start (overlay-start target-ov))
                             (pos-end   (overlay-end target-ov))
                             (updated-text (progn (switch-to-buffer src-buf)
                                                  (get-text-between-2-line-index line-start line-end))))
                        (switch-to-buffer tgt-buf)
                        ;; wipe the overlay contents and repopulate
                        (save-excursion
                          (goto-char pos-start)
                          ;; don't want to directly run delete-region
                          ;; because it causes the overlay to evaporate?
                          ;; or at least becomes hard to track where it
                          ;; moved when the region becomes length 0
                          (insert updated-text)
                          (goto-char (+ pos-start (length updated-text)))
                          (delete-region (point)
                                         (+ -1 (point) (- pos-end pos-start)))
                          (freex-overlay-set-modified-status target-ov nil)))
                    ))
                (switch-to-buffer src-buf))
            ))
        (setq check-buffer-list (cdr check-buffer-list))))))


(add-hook 'after-save-hook 'freex-update-active-overlay-list)

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
          ;; [ ] on save of that source file, overlay is updated
          ;; [X] on save of overlay, source file is updated
          (sweep-and-refresh-matching-overlay source-filepath)
          
          ;; force overlay to be not modified
          (freex-overlay-set-modified-status freex-ov nil)
          (set-buffer-modified-p current-modified-p)
          (message (format "saved %s" (overlay-get freex-ov 'full-filename))))
      (let ((current-filepath (buffer-file-name (current-buffer))))
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
;; make line-start and line-end work
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
                         (buffer-string)))
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

;; NOTE FIXME XXX header parsing is C&P from myembed above
(defun freex-toggle-embed ()
  "prototype embed buffer fn for editing #+INCLUDE files 'in-place'"
  (interactive)
  (if (freex-get-overlay-at-point)
      (freex-close-overlay (point))
    ;; assume we are at a useful header
    (when (save-excursion
            (beginning-of-line 1)
            (looking-at (concat
                         ;; take regexp from org.el:org-edit-special
                         "\\(?:#\\+\\(?:setupfile\\|include\\):?[ \t]+\"?\\|[ \t]*<include\\>.*?file=\"\\)\\([^\"\n>]+\\)"
                         ;; sloppily match :lines ### portion
                         ".+\\(?::lines\\)[ \t]+\\([0-9]*\\)[-~ ]\\([0-9]*\\)")))
      (let* ((file-to-visit (org-trim (match-string 1)))
             (line-start (nonzero-or-nil (match-string 2)))
             (line-end   (nonzero-or-nil (match-string 3))))
        (forward-line 1)
        (freex-create-overlay (point) file-to-visit line-start line-end)))))

(define-key transclude-mode-map (kbd "C-x C-s") 'check-overlay-and-save)
(define-key transclude-mode-map (kbd "C-c E") 'freex-toggle-embed)


(provide 'transclude-mode)


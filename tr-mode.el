;;;###autoload
(define-derived-mode tr-mode text-mode "TR" "Mode for Minetest .tr translation files."
  (setq-local font-lock-defaults '(tr-mode-highlights)))

(defvar tr-mode-map (make-sparse-keymap)
  "Keymap for TR mode.")

(defvar tr-mode-highlights nil "'tr-mode' font-lock faces.")

(setq tr-mode-highlights
	  '(("^#\\(.*\\)$" . 'font-lock-comment-face)
		(".*[^@]=" . 'font-lock-string-face)))

(defun get-current-line-content ()
  "Return the characters of the current line."
  (let ((beg (line-beginning-position))
		(end (line-end-position)))
	(buffer-substring-no-properties beg end)))

(defun line-indentation-position ()
  (save-excursion (back-to-indentation) (point)))

(defun separator-match (content)
  "Return index of start of separator match in CONTENT."
  (string-match-p "[^@]=" content))

(defun comment-p (content)
  "Return t if CONTENT is a comment."
  (equal (string-match-p "^#\\(.*\\)?" content) 0))

(defun entry-p (content)
  "Return non-nil (the index of start of separator match) if CONTENT is a entry."
  (unless (comment-p content)
	(separator-match content)))

(defun translated-p (content)
  (let ((last-index (1- (length content)))
		(sep-index (entry-p content)))
	(unless (equal sep-index nil)
	  (not (= (1+ sep-index) last-index)))))

;;;(defun next-entry ()
;;;  (interactive)
;;;  (while (and (= (forward-line) 0) (not (entry-p))))
;;;  (when (entry-p)
;;;	(re-search-forward "[^@]=")))

;;;(defun previous-entry ()
;;;  (interactive)
;;;  (while (and (= (forward-line -1) 0) (not (entry-p))))
;;;  (when (entry-p)
;;;	(re-search-forward "[^@]=")))

;;;(defun next-untranslated-entry ()
;;;  (interactive)
;;;  (while (progn
;;;		   (next-entry)
;;;		   (translated-p))))


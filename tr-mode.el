;;;###autoload
(define-derived-mode tr-mode text-mode "TR" "Mode for Minetest .tr translation files")

(defvar tr-mode-map (make-sparse-keymap)
  "Keymap for TR mode")

(defun line-indentation-position ()
  (save-excursion (back-to-indentation) (point)))

(defun comment-p ()
  "Returns t if the current line is a comment. Otherwise, nil"
  (= (char-after (line-indentation-position)) ?#))

(defun entry-p ()
  "Returns t if the current line is a entry. Otherwise, nil"
  (let* ((beg (line-beginning-position))
		 (end (line-end-position))
		 (content (buffer-substring-no-properties beg end)))
	(and (not (equal (string-match-p "[^@]=" content) nil)) (not (comment-p)))))

(defun translated-p ()
  "Returns t if the current entry is translated. Otherwise, nil"
  (let* ((beg (line-beginning-position))
		 (end (line-end-position))
		 (content (buffer-substring-no-properties beg end))
		 (last-index (1- (length content))))
	(and (entry-p) (not (= (1+ (string-match-p "[^@]=" content)) last-index)))))

(defun next-entry ()
  (interactive)
  (while (and (= (forward-line) 0) (not (entry-p))))
  (when (entry-p)
	(re-search-forward "[^@]=")))

(defun previous-entry ()
  (interactive)
  (while (and (= (forward-line -1) 0) (not (entry-p))))
  (when (entry-p)
	(re-search-forward "[^@]=")))

(defun next-untranslated-entry ()
  (interactive)
  (while (progn
		   (next-entry)
		   (translated-p))))


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

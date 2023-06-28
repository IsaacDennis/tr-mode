;;;###autoload
(define-derived-mode tr-mode text-mode "TR" "Mode for Minetest .tr translation files."
  (setq-local font-lock-defaults '(tr-mode-highlights)))

(defvar tr-mode-map (make-sparse-keymap)
  "Keymap for TR mode.")

(defvar tr-mode-highlights nil "'tr-mode' font-lock faces.")

(setq tr-mode-highlights
	  '(("^#\\(.*\\)$" . 'font-lock-comment-face)
		(".*[^@]=" . 'font-lock-string-face)))

(defun entry-p (content)
  "Return t if CONTENT is a entry."
  (zerop (string-match-p "^\\([^#].*[^@]\\)=\\(.*\\)?$" content)))

(defun untranslated-p (content)
  "Return t if CONTENT has an untranslated entry."
  (integerp (string-match-p "^[^#\n].+[^@]=$" content)))

(defun next-untranslated-entry ()
  (interactive)
  (when (equal (re-search-forward "^[^#\n].+[^@]=$" nil t) nil)
	(message "No more untranslated entries.")))

(defun previous-untranslated-entry ()
  (interactive)
  (beginning-of-line)
  (if (equal (re-search-backward "^[^#\n].+[^@]=$" nil t) nil)
	  (message "No more untranslated entries.")
	(end-of-line)))

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

(defun buffer-untranslated-p ()
  "Return t if current buffer has an untranslated entry."
  (let ((content (buffer-substring-no-properties (point-min) (point-max))))
	(untranslated-p content)))

(defun check-buffer-untranslated ()
  "Display message if current buffer has an untranslated entry."
  (interactive)
  (if (buffer-untranslated-p)
	  (message "Current buffer contains untranslated entries.")
	(message "No more untranslated entries in current buffer.")))

(defun next-untranslated-entry ()
  (interactive)
  (when (equal (re-search-forward "^[^#\n].+[^@]=$" nil t) nil)
	(message "No more untranslated entries.")))

(defun previous-untranslated-entry ()
  (interactive)
  (let ((beginning-point (point)))
	(beginning-of-line)
	(if (equal (re-search-backward "^[^#\n].+[^@]=$" nil t) nil)
		(progn (message "No more untranslated entries.")
			   (goto-char beginning-point))
	  (end-of-line))))
  
(define-key tr-mode-map (kbd "C-c C-n") #'next-untranslated-entry)
(define-key tr-mode-map (kbd "C-c C-p") #'previous-untranslated-entry)
(define-key tr-mode-map (kbd "C-c C-u") #'check-buffer-untranslated)


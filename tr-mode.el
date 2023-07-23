;;; tr-mode.el --- major mode for Minetest translation files -*- lexical-binding: t -*-

;; Copyright (C) 2023 Isaac Dennis <isaacdennissombra@gmail.com>

;; Author: Isaac Dennis <isaacdennissombra@gmail.com>
;; Maintainer: Isaac Dennis <isaacdennissombra@gmail.com>
;; Created: 2023
;; Version: 0.1
;; Homepage: https://github.com/IsaacDennis/tr-mode
;; Keywords: Minetest, l10n

;; This file is not a part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(defvar tr-mode-map (let ((map (make-sparse-keymap)))
					  (define-key map (kbd "C-c C-n") #'next-untranslated-entry)
					  (define-key map (kbd "C-c C-p") #'previous-untranslated-entry)
					  (define-key map (kbd "C-c C-u") #'check-buffer-untranslated)
					  map)
  "Keymap for TR mode.")

;;;###autoload
(define-derived-mode tr-mode text-mode "TR" "Mode for Minetest .tr translation files."
  (setq-local font-lock-defaults '(tr-mode-highlights)))

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

(provide 'tr-mode)
;;; tr-mode.el ends here

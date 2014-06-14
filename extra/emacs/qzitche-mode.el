;;; qzitche-mode.el --- Major mode for editing qzitche programs

;;; Version: 0.1

;;; License:

;; This file is part of Fronsduk.

;; Fronsduk is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Fronsduk is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Fronsduk.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;; Code:

(defconst qzitche-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\; "<   " st)
    (modify-syntax-entry ?\" "\"   " st)
    st)
  "Syntax table for `qzitche-mode'.")

(defconst qzitche-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `qzitche-mode'.")

(defconst qzitche-keywords-regexp
  (regexp-opt '("fn" "if" "then" "else" "end" "let" "in") 'words)
  "Regexp for qzitche keywords.")

(defconst qzitche-builtin-funcs-regexp
  (regexp-opt '("print" "read") 'words)
  "Regexp for zitche built-in functions.")

(defconst qzitche-font-lock-keywords
  `((,qzitche-keywords-regexp . font-lock-keyword-face)
    (,qzitche-builtin-funcs-regexp . font-lock-function-name-face))
  "Qzitche keywords.")

;;;###autoload
(define-derived-mode qzitche-mode text-mode "qzitche"
  "Major mode for editing qzitche files.

\\{qzitche-mode-map}"
  (set (make-local-variable 'indent-line-function) 'indent-relative)
  (set (make-local-variable 'font-lock-defaults) '(qzitche-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qzt$" . qzitche-mode))

(provide 'qzitche-mode)

;;; qzitche-mode.el ends here

;;; fronsduk-mode.el --- Major mode for editing fronsduk assembler files

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

(defconst fronsduk-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\; "<   " st)
    st)
  "Syntax table for `fronsduk-mode'.")

(defconst fronsduk-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `fronsduk-mode'.")

;;;###autoload
(defconst fronsduk-font-lock-keywords-1
  (cons "\\<\\(?:A\\(?:nd' 'Or\\|p\\)\\|C\\(?:ar\\|dr\\|ons\\)\\|D\\(?:ivide\\|um\\)\\|Eq\\|Join\\|Ld[cf]?\\|Minus\\|N\\(?:il\\|ot\\)\\|P\\(?:lus\\|rint\\)\\|R\\(?:ap\\|ead\\|tn\\)\\|Sel\\|Times\\)\\>"
        '(0 font-lock-builtin-face))
  "Keywords defined in fronsduk assembler.")

(define-derived-mode fronsduk-mode text-mode "fronsduk"
  "Major mode for editing fronsduk assembler files.

\\{fronsduk-mode-map}"
  (set (make-local-variable 'indent-line-function) 'indent-relative)
  (set (make-local-variable 'font-lock-defaults)
       '((fronsduk-font-lock-keywords-1)
         nil
         nil
         nil
         nil)))

(provide 'fronsduk-mode)

;;; fronsduk-mode.el ends here

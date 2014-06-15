;;; fronsduk-mode.el --- Major mode for editing fronsduk assembler files

;;; Version: 0.2

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
    (define-key map (kbd "C-c C-b") 'fronsduk-eval-buffer)
    map)
  "Keymap for `fronsduk-mode'.")

(defgroup fronsduk
  ()
  "Parameters for fronsduk mode."
  :group 'languages)

(defcustom fronsduk-assembler "fronsduk-assemble"
  "Path to fronsduk assembler."
  :group 'fronsduk)

(defcustom fronsduk-vm "fronsduk"
  "Patht o fronsduk virtual machine."
  :group 'fronsduk)

(defun fronsduk-make-fdk-file-name (file-name)
  "Create a temporary bytecode file for FILE-NAME."
  (concat (file-name-sans-extension file-name) "-" (format "%s" (random 1e6)) ".fdk"))

(defun fronsduk-eval-buffer ()
  "Compile and run the content of the current buffer in a virtual machine."
  (interactive)
  (let ((file-name (file-name-nondirectory (buffer-file-name))))
    (when file-name
      (let ((fdk-file-name (fronsduk-make-fdk-file-name file-name))
            (buffer (get-buffer-create "*fronsduk-eval*")))
        (with-current-buffer buffer (erase-buffer))
        (async-shell-command (concat fronsduk-assembler
                                     " <" file-name
                                     " >" fdk-file-name
                                     " && " fronsduk-vm " " fdk-file-name
                                     " && rm " fdk-file-name)
                             buffer
                             buffer)))))

(defconst fronsduk-font-lock-keywords-1
  (cons "\\<\\(?:A\\(?:nd' 'Or\\|p\\)\\|C\\(?:ar\\|dr\\|ons\\)\\|D\\(?:ivide\\|um\\)\\|Eq\\|Join\\|Ld[cf]?\\|Minus\\|N\\(?:il\\|ot\\)\\|P\\(?:lus\\|rint\\)\\|R\\(?:ap\\|ead\\|tn\\)\\|Sel\\|Times\\)\\>"
        '(0 font-lock-builtin-face))
  "Keywords defined in fronsduk assembler.")

;;;###autoload
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

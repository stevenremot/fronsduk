;;; qzitche-mode.el --- Major mode for editing qzitche programs

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

(defconst qzitche-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\; "<   " st)
    (modify-syntax-entry ?\" "\"   " st)
    st)
  "Syntax table for `qzitche-mode'.")

(defconst qzitche-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-b") 'qzitche-eval-buffer)
    (define-key map (kbd "C-c C-d") 'qzitche-disassemble-buffer)
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

(defgroup qzitche
  ()
  "Parameters for qzitche mode."
  :group 'languages)

(defcustom qzitche-fronsduk-vm "fronsduk"
  "Path to fronsduk virtual machine."
  :group 'qzitche)

(defcustom qzitche-compiler "qzitchec"
  "Path to qzitche compiler."
  :group 'qzitche)

(defcustom qzitche-disassembler "fronsduk-disassemble"
  "Path to bytecode disassembler."
  :group 'qzitche)

(defun qzitche-make-fdk-file-name (file-name)
  "Create a temporary bytecode file for FILE-NAME."
  (concat (file-name-sans-extension file-name) "-" (format "%s" (random 1e6)) ".fdk"))

(defun qzitche-eval-buffer ()
  "COmpile and run the content of the current buffer in a virtual machine."
  (interactive)
  (let ((file-name (file-name-nondirectory (buffer-file-name))))
    (when file-name
      (let ((fdk-file-name (qzitche-make-fdk-file-name file-name))
            (buffer (get-buffer-create "*qzitche-eval*")))
        (with-current-buffer buffer (erase-buffer))
        (async-shell-command (concat qzitche-compiler
                                     " <" file-name
                                     " >" fdk-file-name
                                     " && " qzitche-fronsduk-vm " " fdk-file-name
                                     " && rm " fdk-file-name)
                             buffer
                             buffer)))))

(defun qzitche-disassemble-buffer ()
  "Convert the content of the current buffer to assembler."
  (interactive)
  (let ((file-name (file-name-nondirectory (buffer-file-name))))
    (when file-name
      (let ((fdk-file-name (qzitche-make-fdk-file-name file-name))
            (buffer (get-buffer-create "*qzitche-asm*")))
        (with-current-buffer buffer
          (erase-buffer)
          (shell-command (concat qzitche-compiler
                                 " <" file-name
                                 " >" fdk-file-name
                                 " && " qzitche-disassembler " <" fdk-file-name
                                 " && rm " fdk-file-name)
                         buffer
                         buffer)
          (display-buffer buffer 'display-buffer-pop-up-window)
          (when (fboundp 'fronsduk-mode)
            (fronsduk-mode)))
        ))))

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

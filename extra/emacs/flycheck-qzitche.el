;;; flycheck-qzitche.el --- A flycheck syntax checker for Qzitche
;;; Version: 0.0.1

;;; Commentary:
;;
;;; Code:
(require 'flycheck)

;;;###autoload
(eval-after-load 'flycheck
  '(progn
     (flycheck-define-checker qzitche-qzitchec
       "A qzitche syntax checker using base compiler."
       :command ("qzitchec" source)
       :error-patterns ((error
                         line-start
                         "qzitchec: "
                         (opt "Parsing failed : ") "(line " line ", column " column "): "
                         (message)
                         line-end))
       :modes qzitche-mode)

     (add-to-list 'flycheck-checkers 'qzitche-qzitchec)))

(provide 'flycheck-qzitche)

;;; flycheck-qzitche.el ends here

;;; my-diminish.el --- diminish mode-line -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; すでに Prelude/core でロードされているパッケージを
;; modeline から diminish するためのファイル
;;

;;; Code:

(defmacro safe-diminish (file mode &optional new-name)
  "https://github.com/larstvei/dot-emacs/blob/master/init.org"
  `(with-eval-after-load ,file
     (diminish ,mode ,new-name)))

(safe-diminish "abbrev" 'abbrev-mode)
(safe-diminish "auto-complete" 'auto-complete-mode)
(safe-diminish "eldoc" 'eldoc-mode)
(safe-diminish "flycheck" 'flycheck-mode)
(safe-diminish "flyspell" 'flyspell-mode)
(safe-diminish "helm-mode" 'helm-mode)
(safe-diminish "paredit" 'paredit-mode)
(safe-diminish "projectile" 'projectile-mode)
(safe-diminish "rainbow-mode" 'rainbow-mode)
(safe-diminish "simple" 'auto-fill-function)
(safe-diminish "smartparens" 'smartparens-mode)
(safe-diminish "smooth-scroll" 'smooth-scroll-mode)
(safe-diminish "undo-tree" 'undo-tree-mode)
(safe-diminish "volatile-highlights" 'volatile-highlights-mode)
(safe-diminish "company" 'company-mode)
(safe-diminish "whitespace" 'whitespace-mode)
(safe-diminish "guru-mode" 'guru-mode)
(safe-diminish "which-key" 'which-key-mode)



(provide 'my-diminish)
;;; my-diminish.el ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-async-byte-compile
(use-package auto-async-byte-compile
  :disabled t
  :config
  (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
;;; to suppress warnings when byte-compling
  ;; (setq byte-compile-warnings
  ;;       '(free-vars unresolved callargs redefine obsolete noruntime
  ;;                   cl-functions interactive-only make-local))
)

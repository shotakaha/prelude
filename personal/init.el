;;; my-init.el -- my-init.el

;;; Commentary
;; My init.el that goes with Emacs Prelude.
;; First, install and setup Emacs Prelude.
;; Then modify elisps in personal/ directory.
;;
;; I will use "use-package.el" instead of "require".
;; Transition is still on away.
;;
;; Also, I am using "init-loader.el" right now,
;; but i will try to depend less upon it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enlarge garbage collection
(setq gc-cons-threshold (* 128 1024 1024 1024))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load-theme
(load-theme 'manoj-dark t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load newer file
(setq load-prefer-newer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; use-package.el
;;; http://rubikitch.com/2014/09/09/use-package/
;;; https://github.com/jwiegley/use-package
;;; http://qiita.com/kai2nenobu/items/5dfae3767514584f5220
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package.el
(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (fset 'package-desc-vers 'package--ac-desc-version)
  (package-initialize)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-loader.el
(use-package init-loader
  :ensure t
  :config
  ;;; always show *init log* after starting emacs
  ;; (setq init-loader-show-log-after-init t)

  ;;; show *init log* when there were error
  (setq init-loader-show-log-after-init 'error-only)

  ;;; set directory for init-loader
  (init-loader-load "~/.emacs.d/personal/inits/")

  ;;; always byte-compile when loading
  (setq init-loader-byte-compile t)
  )


(provide 'init)
;;; init.el ends here

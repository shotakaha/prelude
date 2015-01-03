;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; use-package.el
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package.el
(use-package package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(fset 'package-desc-vers 'package--ac-desc-version)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-loader.el
(use-package init-loader)
;; (setq init-loader-show-log-after-init t) ;;; ロード結果を常に表示させたい場合（*init log*)
(setq init-loader-show-log-after-init 'error-only) ;;; エラーがあったときだけ、*init log* を作成
(init-loader-load "~/.emacs.d/personal/inits/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load-theme
(load-theme 'manoj-dark t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load newer file
;;; (setq load-prefer-newer t)

(provide 'init)
;;; init.el ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package.el
;;; 自動インストール

;; (require 'cl)

;; (defvar installing-package-list
;;   '(
;;     ;;; ここにパッケージを書く
;;     ))

;; (let ((not-installed (loop for x in installing-package-list
;;                            when (not (package-installed-p x))
;;                            collect x)))
;;   (when not-installed
;;     (package-refresh-contents)
;;     (dolist (pkg not-installed)
;;       (package-install pkg))))

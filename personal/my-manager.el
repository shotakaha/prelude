;;; my-manager.el --- my manager config

;;; Commentary
;; ID管理パッケージの設定

;; 1. id-manager.el
;; 2. passthword.el


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; id-manager.el
;;; https://github.com/kiwanami/emacs-id-manager
(use-package id-manager
  :config
  (setq idm-database-file "~/.emacs.d/idm-db.gpg")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; passthword
(setq epa-file-cache-passphrase-for-symmetric-encryption t)


(provide my-manager)
;;; my-manager.el ends here

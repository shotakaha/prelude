;;; my-manager.el --- my manager config

;;; Commentary
;; ID管理パッケージの設定

;; GnuPGのインストールが必要
;; $ sudo port install gnupg

;; 1. id-manager.el
;; 2. passthword.el

(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; id-manager.el
;;; https://github.com/kiwanami/emacs-id-manager
;;; Helm式パスワードマネージャ
;;; コメントを付けれるので、こっちの方がいいのかも
(use-package id-manager
  :ensure t
  :config
  (setq idm-database-file "~/.emacs.d/idm-db.gpg")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; passthword
;;; Ido式パスワードマネージャ
;;; Helmを使わないので操作が単純
(use-package passthword
  :ensure t
  )

(provide 'my-manager)
;;; my-manager.el ends here

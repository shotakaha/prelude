;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; migemo.el
;;; http://rubikitch.com/2014/08/20/migemo/

(use-package migemo
  :disabled t
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))

  ;; migemoはcmigemoに依存しているらしい
  ;; が、MacOSはどうしたらいいのか不明
  ;;  MacPortsにはない

  ;; Set your installed path
  ;; (setq migemo-dictionary "path/to/migemo/library")

  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf8-unix)
  (load-library "migemo")
  (migemo-init)
)

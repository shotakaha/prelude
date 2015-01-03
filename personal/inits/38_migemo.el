;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; migemo.el
;;; http://rubikitch.com/2014/08/20/migemo/
;;; http://weblog.ymt2.net/blog/html/2013/08/23/install_migemo_to_emacs_24_3_1.html

(use-package migemo
  :disabled t
  :config
  (setq migemo-command "/usr/local/bin/cmigemo")
  (setq migemo-options '("-q" "--emacs"))

  ;; migemoはcmigemoに依存しているらしい
  ;; が、MacOSはどうしたらいいのか不明
  ;; MacPortsにはない -> github からインストールした

  ;; Set your installed path
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf8-unix)
  (load-library "migemo")
  (migemo-init)
)

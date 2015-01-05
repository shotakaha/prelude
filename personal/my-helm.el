;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-swoop.el
;;; http://rubikitch.com/2014/12/25/helm-swoop/
(prelude-require-packages '(helm-swoop helm-migemo ace-isearch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-migemo.el
;;; http://rubikitch.com/2014/12/19/helm-migemo/
(prelude-require-package 'helm-migemo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; migemo.el
;;; http://rubikitch.com/2014/08/20/migemo/
;;; First, get cmigemo from http://www.kaoriya.net/software/cmigemo/
;;; See http://weblog.ymt2.net/blog/html/2013/08/23/install_migemo_to_emacs_24_3_1.html
(prelude-require-package 'migemo)

(use-package migemo
  :disabled t
  :config
  (setq migemo-command "/usr/local/bin/cmigemo")
  (setq migemo-options '("-q" "--emacs"))

  ;; Set your installed path
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf8-unix)
  (load-library "migemo")
  (migemo-init)
  )

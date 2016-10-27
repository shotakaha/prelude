;;; preload/init.el -- init.el

;;; Commentary:
;; init.el that does not depend on any additional packages.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check OS
(defvar oldemacs-p (<= emacs-major-version 22)) ; <= Emacs22
(defvar emacs23-p (<= emacs-major-version 23))  ; <= Emacs23
(defvar emacs24-p (>= emacs-major-version 24))  ; >= Emacs24
(defvar linux-p (eq system-type 'gnu/linux))    ; for Linux
(defvar darwin-p (eq system-type 'darwin))      ; for Mac OS X
(defvar nt-p (eq system-type 'windows-nt))      ; for Windows

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load-theme
;;; manoj-dark : 背景:黒、文字:橙
;;; leuven     : 背景:白、文字:黒、
;;;              Org-modeの見出しが大きく表示されてGOOD!
(when linux-p
  (message "OS : GNU/Linux")
  (setq prelude-theme 'manoj-dark)
  (setq skk-tut-file "/usr/share/skk/SKK.tut")
  )
(when darwin-p
  (message "OS : MacOS X")
  (setq prelude-theme 'leuven)
;;;  (setq prelude-theme 'manoj-dark)
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Always start emacs daemon
;;; emacsclient を使って接続する
;;; /Applications/Emacs.app/以下のでも、
;;; /opt/local/bin/emacsclient でもどちらでもよい
;;; .bashrcに、export EDITOR=emacsclient としておくとよい
;;; server-start の重複防止
;;; http://syohex.hatenablog.com/entry/20140928/1411874248
(require 'server)
(unless (server-running-p)
  (server-start)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Confirm when Quit Emacs
(setq confirm-kill-emacs 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; garbage collection
;;; threshold 128MB
;;; leave message when gc collected
(setq gc-cons-threshold (* 128 1024 1024))
(setq garbage-collection-messages t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load newer file
(setq load-prefer-newer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; always truncate lines (toggle with M-l)
(set-default 'truncate-lines t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; display date & time in 24hrs format
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time)

;;; key-bindings are written in ../init.el using use-package

(provide 'preload-init)
;;; preload/init.el ends here

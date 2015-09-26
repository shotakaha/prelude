;;; preload/init.el -- init.el

;;; Commentary:
;; init.el that does not depend on any additional packages.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Always start emacs daemon
;;; emacsclient を使って接続する
;;; /Applications/Emacs.app/以下のでも、
;;; /opt/local/bin/emacsclient でもどちらでもよい
;;; .bashrcに、export EDITOR=emacsclient としておくとよい
(server-start)

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
;;; show line number
(global-linum-mode 1)

;;; key-bindings are written in ../init.el using use-package

;;; preload/init.el ends here

;;; personal/init.el -- My init.el

;;; Commentary:
;; My init.el that goes with Emacs Prelude.
;; First, install and setup Emacs Prelude.
;; Then modify elisps in personal/ directory.
;;
;; I will use "use-package.el" instead of "require".
;; Transition is still on away.
;;
;; Also, I am using "init-loader.el" right now,
;; but i will try to depend less upon it.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; generic-x.el
;;; http://rubikitch.com/2014/08/03/blog/
;;; http://drunkard-diogenes.blogspot.jp/2014/07/emacs-lisp.html
(require 'generic-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; use-package.el
;;; http://rubikitch.com/2014/09/09/use-package/
;;; https://github.com/jwiegley/use-package
;;; http://qiita.com/kai2nenobu/items/5dfae3767514584f5220
(require 'use-package)
(setq use-package-verbose t)
(setq use-package-minimum-reported-time 0.001)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-loader.el
(use-package init-loader
  :disabled t
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C-x 3 C-x o --> C-t
;;; 画面の移動を簡単に行う方法
;;; （画面が分割されてない場合は、２分割(C-x 3)する）
;;; copied from http://d.hatena.ne.jp/rubikitch/20100210/emacs
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
;; transpose-char(C-t) は普段使わないのでつぶす
(bind-key* "C-t" 'other-window-or-split)
(bind-key* "C-c t" 'other-window-or-split)
(bind-key "s-{" 'other-window-or-split)
(bind-key "s-}" 'other-window-or-split)
;;(bind-key* "C-u C-t" 'delete-other-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; passthword
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; key-bindings
(bind-key "C-h" 'delete-backward-char)
(bind-key "C-x m" 'smex)


(provide 'init)
;;; init.el ends here

;;; my-init.el -- my-init.el

;;; Commentary
;; My init.el that goes with Emacs Prelude.
;; First, install and setup Emacs Prelude.
;; Then modify elisps in personal/ directory.
;;
;; I will use "use-package.el" instead of "require".
;; Transition is still on away.
;;
;; Also, I am using "init-loader.el" right now,
;; but i will try to depend less upon it.

;;; Code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; garbage collection
;;; threshold 128MB
;;; leave message when gc collected
(setq gc-cons-threshold (* 128 1024 1024))
(setq garbage-collection-messages t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load-theme
;; (load-theme 'manoj-dark t)
(load-theme 'leuven t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load newer file
(setq load-prefer-newer t)

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package.el
(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (fset 'package-desc-vers 'package--ac-desc-version)
  (package-initialize)
  )

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
;;; uniquify.el
;;; Easy to recognize same-named file in different directory
;;; Already set in core/prelude-editor.el
;;; replace some variables
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; key-bind
(bind-key "C-h" 'delete-backward-char)

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
;;(bind-key* "C-u C-t" 'delete-other-window)


(provide 'init)
;;; init.el ends here

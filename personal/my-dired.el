;;; my-dired.el --- my Dired-mode configuration

;;; Commentary:

;; === What is Dired-mode ===
;; A dired-mode is enabled when a buffer has directories/subdirectories.
;; You can use "Dired commands" in this mode.

;; === Usage of Dired ===
;; C-x C-f <directory>, or C-x d to call dired.el
;; Press <RET> to open that file.
;; Press <*> to mark multiple files
;; Press <u> to unmark

;; === Dired commands ===
;; C : dired-do-copy
;; D : dired-do-delete
;; R : dired-do-rename
;; S : dired-do-symlink
;; H : dired-do-hardlink
;; M : dired-do-chmod
;; O : dired-do-chown
;; G : dired-do-chgrp
;; L : dired-do-load
;; B : dired-do-byte-compile
;; X : dired-do-shell-command
;; g : revert-buffer (update dired buffer)
;; ^ : dired-up-directory
;; %m : dired-mark-files-regexp (filename matches regexp)
;; %g : dired-mark-files-containing-regexp (contants in file matches regexp)

;; === Wdired ===
;; When renaming files, it is better to use wdired-mode
;; M-x wdired-change-to-wdired-mode : enable edit Dired buffer (<Editable Dired> in modeline)
;; Press C-c C-c or C-x C-s to finish. (M-x wdired-finish-edit)
;; Press C-c C-k to abort changes. (M-x wdired-abort-changes)

(use-package dired
  :bind (:map dired-mode-map
              ("r" . wdired-change-to-wdired-mode)
              ("l" . dired-omit-mode)
              ("(" . dired-hide-details-mode)
              (")" . dired-hide-details-mode)
              )
  :config
  (setq dired-isearch-filenames t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; direx.el --- Yet Another Dired
;;; http://rubikitch.com/2014/10/18/direx/
;;;
(use-package direx
  :disabled t
  :ensure t
  :bind (("C-x C-j" . direx:jump-to-directory)
         ("C-\\" . direx-project:jump-to-project-root-other-window)
         )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dired-k.el ---  highlight dired buffer
;;; for dired, use dired-k
;;; for direx, use direx-k
;;; http://rubikitch.com/2014/10/19/dired-k/
;;; https://github.com/syohex/emacs-dired-k
(use-package dired-k
  :ensure t
  :bind (:map dired-mode-map
              ("K" . dired-k)
              ("g" . dired-k)
              )
  :config
  ;; always open dired with dired-k
  (add-hook 'dired-initial-position-hook 'dired-k)
  )

(use-package direx-k
  :disabled t
  :bind ("C-\\" . direx-project:jump-to-project-root-other-window)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dired-toggle.el
;;; http://rubikitch.com/2014/09/08/dired-toggle/
(use-package dired-toggle :disabled t :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; async.el
;;; http://rubikitch.com/2014/09/20/dired-async/
(use-package async
  :ensure t
  :config
  (eval-after-load "dired-aux" '(require 'dired-async))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wgrep.el - http://rubikitch.com/2014/09/13/wgrep/
;;; ag.el - http://rubikitch.com/2014/09/12/ag/
;; (prelude-require-packages '(wgrep ag))
;; ;;; eでwgrepモードにする
;; (setf wgrep-enable-key "e")
;; ;;; wgrep終了時にバッファを保存
;; (setq wgrep-auto-save-buffer t)
;; ;;; read-only bufferにも変更を適用する
;; (setq wgrep-change-readonly-file t)

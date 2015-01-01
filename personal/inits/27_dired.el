;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dired
;;; C-x C-f <directory>, or C-x d to call dired.el
;;; Press <RET> to open that file.
;;; Press <*> to mark multiple files, <u> to unmark
;;; Then, press keys below to operate files.
;;; C : dired-do-copy
;;; D : dired-do-delete
;;; R : dired-do-rename
;;; S : dired-do-symlink
;;; H : dired-do-hardlink
;;; M : dired-do-chmod
;;; O : dired-do-chown
;;; G : dired-do-chgrp
;;; L : dired-do-load
;;; B : dired-do-byte-compile
;;; X : dired-do-shell-command
;;; g : revert-buffer (update dired buffer)
;;; ^ : dired-up-directory
;;; %m : dired-mark-files-regexp (filename matches regexp)
;;; %g : dired-mark-files-containing-regexp (contants in file matches regexp)
(setq dired-dwim-target t)
(setq dired-recursive-copies 'always)
(setq dired-isearch-filenames t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wdired setting
;;; (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
;;; Press <r> in dired buffer. (<Editable Dired> in modeline)
;;; Press C-c C-c or C-x C-s to finish. (wdired-finish-edit)
;;; Press C-c C-k to abort changes. (wdired-abort-changes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dired-k.el
(require 'dired)
(add-hook 'dired-initial-position-hook 'dired-k)

(use-package direx-k
  :disabled t
;; (global-set-key (kbd "C-\\") 'direx-project:jump-to-project-root-other-window)
;; (define-key direx:direx-mode-map (kbd "K") 'direx-k)
  )


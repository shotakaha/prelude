;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stripe-buffer.el
;;; http://rubikitch.com/2014/11/30/stripe-buffer/
(prelude-require-package 'stripe-buffer)
(use-package stripe-buffer
  :config
  (add-hook 'dired-mode-hook 'stripe-listify-buffer)
  (add-hook 'org-mode-hook 'turn-on-stripe-table-mode)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dired-toggle.el
;;; http://rubikitch.com/2014/09/08/dired-toggle/
(prelude-require-package 'dired-toggle)
;; no need for additional setting.
;; just type M-x dired-toggle

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dired-details.el
;;; http://rubikitch.com/2014/09/08/dired-details/
;;; Commentary
;;; dired is already loaded by Prelude.
(prelude-require-package 'dired-details)
(use-package dired-details
  :config
  (dired-details-install)
  (setq dired-details-hidden-string "")
  (setq dired-details-hide-link-targets nil)

  (defadvice find-dired-sentinel (after dired-details (proc state) activate)
    "find-diredでもdired-detailsを使えるようにする"
    (ignore-errors
      (with-current-buffer (process-buffer proc)
        (dired-details-activate))))
  )


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

(use-package direx-k
  :disabled t
  (add-hook 'dired-initial-position-hook 'dired-k)
  ;; (global-set-key (kbd "C-\\") 'direx-project:jump-to-project-root-other-window)
  ;; (define-key direx:direx-mode-map (kbd "K") 'direx-k)
  )

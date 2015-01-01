;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; view-mode
(use-package view)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enable view-mode
;; Open with 'C-x C-r'
;; Toggle with 'C-x C-q'
(setq view-read-only t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; viewer.el --- improve view-mode
(use-package viewer
  :config
  (viewer-stay-in-setup)
;;; set color to mode-line
  (setq viewer-modeline-color-unwritable "tomato")
  (setq viewer-modeline-color-view "orange")
  (viewer-change-modeline-color-setup)
;;; set view-mode kbd according to major-mode
  (define-overriding-view-mode-map c-mode
    ("RET" . gtags-find-tag-from-here))
  (define-overriding-view-mode-map emacs-lisp-mode
    ("RET" . find-function-at-point))
;;; open specific file in view-mode
  (setq view-mode-by-default-regexp "\\.log$")
;;;(viewer-aggressive-setup 'force)    ;;; very very aggressive
)


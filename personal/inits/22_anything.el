;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything.el
(use-package anything-startup
  :config
  (define-key anything-map "\C-\M-p" 'anything-previous-source)
  (define-key anything-map "\C-\M-n" 'anything-next-source)
  (setq descbinds-anything-window-style 'split-window)
;;; written in keybinds.el
  ;; (global-set-key (kbd "C-x :") 'anything-for-files)
  ;; (global-set-key (kbd "M-y") 'anything-show-kill-ring)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything-moccur.el
;;; combine "isearch" and "occur"
;;(use-package anything-c-moccur)

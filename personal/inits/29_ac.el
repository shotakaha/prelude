;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-complete
;;; auto-complete.el
(use-package auto-complete-config
  :config
  (ac-config-default)
  (global-auto-complete-mode t)
  ;; (define-key ac-complete-mode-map "\C-n" 'ac-next)
  ;; (define-key ac-complete-mode-map "\C-p" 'ac-previous)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ac-ispell.el
;;; auto-complete words longer than 4 characters
(custom-set-variables
 '(ac-ispell-requires 4)
 '(ac-ispell-fuzzy-limit 4))

(eval-after-load "auto-complete"
  '(progn
     (ac-ispell-setup)))

(add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
(add-hook 'mail-mode-hook 'ac-ispell-ac-setupa)

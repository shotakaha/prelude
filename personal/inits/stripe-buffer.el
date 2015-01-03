;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stripe-buffer.el
;;; http://rubikitch.com/2014/11/30/stripe-buffer/

(use-package stripe-buffer
  :config
  (add-hook 'dired-mode-hook 'stripe-listify-buffer)
  (add-hook 'org-mode-hook 'turn-on-stripe-table-mode)
)

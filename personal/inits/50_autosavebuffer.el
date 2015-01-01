;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-save-buffers-enhanced.el
(use-package auto-save-buffers-enhanced
  :config
  (auto-save-buffers-enhanced t)
)

;;; If you want to specify the files explicitly using regex.
;; (setq auto-save-buffers-enhanced-include-regexps '(".+"))
;; (setq auto-save-buffers-enhanced-exclude-regexps '("^not-save-file" "\\.ignore$"))

;;; If you're using CVS or Subversion or git
;; (require 'auto-save-buffers-enhanced)
;; (auto-save-buffers-enhanced-include-only-checkout-path t)
;; (auto-save-buffers-enhanced t)

;;; If you're using also svk
;; (require 'auto-save-buffers-enhanced)
;; (setq auto-save-buffers-enhanced-use-svk-flag t)
;; (auto-save-buffers-enhanced-include-only-checkout-path t)
;; (auto-save-buffers-enhanced t)

;;; You can toggle `auto-save-buffers-enhanced' activity to execute
;;; `auto-save-buffers-enhanced-toggle-activity'. For convinience, you
;;; might want to set keyboard shortcut of the command like below:
;; (global-set-key (kbd "C-x a s") 'auto-save-buffers-enhanced-toggle-activity)

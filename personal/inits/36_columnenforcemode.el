;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; column-enforce-mode.el
(require 'column-enforce-mode)
(defun text-mode-hook--column-enforce-mode()
  (set (make-local-variable 'column-enforce-column) 50)
  (column-enforce-mode 1))
(add-hook 'text-mode-hook 'text-mode-hook--column-enforce-mode)

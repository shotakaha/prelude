;;; my-root-help.el --- My ROOT configuration

;;; Commentary:

;;; Code:

(use-package root-help
  :load-path "/opt/local/libexec/root6/share/emacs/site-lisp/"
  :config
  (bind-keys :map c++-mode-map
             ("C-c C-r c" . root-class)
             ("C-c C-r h" . root-header-skel)
             ("C-c C-r s" . root-source-skel)
             ("C-c C-r i" . root-include-header)
             ("C-c C-r m" . root-main)
             ("C-c C-r l" . root-insert-linkdef)
             ("C-c C-r p" . root-insert-pragma)
             ("C-c C-r x" . root-shell)
             ("C-c C-r g" . root-send-line-to-root)
             ("C-c C-r r" . root-send-region-to-root)
             ("C-c C-r b" . root-send-buffer-to-root)
             ("C-c C-r f" . root-execute-file)
             )
  )

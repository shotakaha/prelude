;;; my-core.el

;;; Commentary:
;; Configurations for default Emacs packages or
;; the packages already required in previous elisp.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; uniquify.el
;;; Easy to recognize same-named file in different directory
;;; Already set in core/prelude-editor.el
;;; replace some variables
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;;; my-core.el ends here

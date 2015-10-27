;;; my-ui.el --- My UI configuration

;;; Commentary:

;; smart-mode-line
;;  - already defined in ../core/prelude-ui.el
;;  - add extra

;;; Code:

(display-time-mode 1)
(display-battery-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; smart-mode-line
;;; already defined in core/prelude-ui.el
;;; https://github.com/Malabarba/smart-mode-line
;;; rich-minority
;;; https://github.com/Malabarba/rich-minority
;;; M-x describe-variable minor-mode-list

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'automatic
        sml/shorten-directory t
        sml/name-width 16
        sml/shorten-modes t
        sml/use-projectile-p 'before-prefixes
        sml/projectile-replacement-format "%s/"
        sml/read-only-char "%%")
  (sml/setup)
)

(use-package rich-minority
  :ensure t
  :config
  (setq rm-whitelist t)
  )

(provide 'my-ui)
;;; my-ui.el ends here

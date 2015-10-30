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
;;; M-x describe-variable sml/replacer-regexp-list

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
  ;; (add-to-list 'sml/replacer-regexp-list '("^~/public_html/dokuwiki" ":MyWiki:") t)
  (sml/setup)
)

(use-package rich-minority
  :disabled t
  :ensure t
  :config
  (setq rm-whitelist (mapconcat 'identity list-of-regexps "\\|"))
  )

(provide 'my-ui)
;;; my-ui.el ends here

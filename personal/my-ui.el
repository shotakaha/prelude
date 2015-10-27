;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; smart-mode-line
;;; https://github.com/Malabarba/smart-mode-line
;;; rich-minority
;;; https://github.com/Malabarba/rich-minority
;;; M-x describe-variable minor-mode-list
;;; already defined in core/prelude-ui.el

(use-package smart-mode-line
  :disabled t
  :ensure t
  ;; :init
  ;; (progn
  ;;   (setq sml/theme 'automatic
  ;;         sml/shorten-directory t
  ;;         sml/name-width 16
  ;;         sml/shorten-modes t
  ;;         sml/use-projectile-p 'before-prefixes
  ;;         sml/projectile-replacement-format "%s/"
  ;;         sml/read-only-char "%%")
  ;;   ;; (setq rm-whitelist t)
  ;;   ;; (setq rm-blacklist (mapconcat 'identity list-of-regexps "\\|"))
  ;;   (sml/setup)
  ;;   )
  :config
  (setq sml/theme 'automatic
        sml/shorten-directory t
        sml/name-width 16
        sml/shorten-modes t
        sml/use-projectile-p 'before-prefixes
        sml/projectile-replacement-format "%s/"
        sml/read-only-char "%%")
  ;; (setq rm-whitelist t)
  ;; (setq rm-blacklist (mapconcat 'identity list-of-regexps "\\|"))
  (sml/setup)
)

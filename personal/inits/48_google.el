;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; google.el --- Emacs interface to the Google API
(require 'google)
;;(setq google-license-key "optional")
;;(setq google-referer "my url")
;;(google-search-video "rickroll")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; google-c-style.el
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; google-this.el ---
;;; google-this is a package that provides a set of functions and
;;; keybindings for launching google searches from within Emacs.
;;; ---
;;; The main function is `google-this' (bound to C-c / g).
;;; It does a google search using the currently selected region,
;;; or the expression under point.
;;; ---
;;; All functions are bound under "C-c /" prefix,
;;; in order to comply with Emacs' standards.
;;; If that's a problem see `google-this-keybind'.
;;; To view all keybindings type "C-c / C-h".
(require 'google-this)
(google-this-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; google-translage.el ---
;;; Emacs interface to Google Translate.
;;; UIが違うらしいのでどちらかえらぶ

;;(require 'google-translate)
;; (require 'google-translate-default-ui)
;; (global-set-key (kbd "C-c t") 'google-translate-at-point)
;; (global-set-key (kbd "C-c T") 'google-translate-query-translate)

(require 'google-translate)
(require 'google-translate-smooth-ui)
(global-set-key (kbd "C-c t") 'google-translate-smooth-translate)

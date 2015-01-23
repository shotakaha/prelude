;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; google.el --- Emacs interface to the Google API
;; (use-package google)
;; ;;(setq google-license-key "optional")
;; ;;(setq google-referer "my url")
;; ;;(google-search-video "rickroll")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; google-c-style.el
;; (use-package google-c-style
;;   :config
;;   (add-hook 'c-mode-common-hook 'google-set-c-style)
;;   (add-hook 'c-mode-common-hook 'google-make-newline-indent)
;; )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; google-this.el ---
;; ;;; google-this is a package that provides a set of functions and
;; ;;; keybindings for launching google searches from within Emacs.
;; ;;; ---
;; ;;; The main function is `google-this' (bound to C-c / g).
;; ;;; It does a google search using the currently selected region,
;; ;;; or the expression under point.
;; ;;; ---
;; ;;; All functions are bound under "C-c /" prefix,
;; ;;; in order to comply with Emacs' standards.
;; ;;; If that's a problem see `google-this-keybind'.
;; ;;; To view all keybindings type "C-c / C-h".
;; (use-package google-this
;;   :config
;;   (google-this-mode 1)
;; )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; google-translage.el ---
;; ;;; Emacs interface to Google Translate.
;; ;;; UIが違うらしいのでどちらかえらぶ
;; ;;; http://rubikitch.com/2014/12/07/google-translate/
;; ;;; http://rubikitch.com/2014/12/30/google-translate-sentence/

;; ;;(require 'google-translate)
;; ;; (require 'google-translate-default-ui)
;; ;; (global-set-key (kbd "C-c t") 'google-translate-at-point)
;; ;; (global-set-key (kbd "C-c T") 'google-translate-query-translate)


;; (use-package google-translate
;;   :config
;;   (defvar google-translate-english-chars "[:ascii:]"
;;     "これらの文字が含まれているときは英語とみなす")
;;   (defun google-translate-enja-or-jaen (&optional string)
;;     "regionか、現在のセンテンスを言語自動判別でGoogle翻訳する。"
;;     (interactive)
;;     (setq string
;;           (cond ((stringp string) string)
;;                 (current-prefix-arg
;;                  (read-string "Google Translate: "))
;;                 ((use-region-p)
;;                  (buffer-substring (region-beginning) (region-end)))
;;                 (t
;;                  (save-excursion
;;                    (let (s)
;;                      (forward-char 1)
;;                      (backward-sentence)
;;                      (setq s (point))
;;                      (forward-sentence)
;;                      (buffer-substring s (point)))))))
;;     (let* ((asciip (string-match
;;                     (format "\\`[%s]+\\'" google-translate-english-chars)
;;                     string)))
;;       (run-at-time 0.1 nil 'deactivate-mark)
;;       (google-translate-translate
;;        (if asciip "en" "ja")
;;        (if asciip "ja" "en")
;;        string)))
;;   (global-set-key (kbd "C-c t") 'google-translate-enja-or-jaen)
;;   )

;; ;; (use-package google-translate-smooth-ui)
;; ;; (global-set-key (kbd "C-c t") 'google-translate-smooth-translate)

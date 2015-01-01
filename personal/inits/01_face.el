;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; face
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set Ricty as default font
;; (set-face-attribute 'default nil
;;                     :family "Ricty"
;;                     :height 160)
;; (set-fontset-font "fontset-default"
;;                   'japanese-jisx0208
;;                   '("Ricty" . "iso10646-*"))

;;(set-frame-font "Ricty-21")
(add-to-list 'default-frame-alist '(font . ("Ricty-21"))

;; (setq initial-frame-alist
;;       (append (list
;;                '(width . 45)
;;                '(height . 45)
;;                '(top . 0)
;;                '(left . 0)
;;                '(font . "Ricty-16")
;;                )
;;               initial-frame-alist))
;; (setq default-frame-alist initial-frame-alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vline.el
(require 'vline)
(vline-global-mode 1)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; highlight indent
(require 'col-highlight)
(column-highlight-mode 1)                ;; always highlight
;;(toggle-highlight-column-when-idle 1)   ;; highlight when idle
(col-highlight-set-interval 6)
;; highlight colummn
(require 'crosshairs)
(crosshairs-flash 1)    ;; always enabled


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vline.el
(use-package vline
  :config
  (vline-global-mode 1)
)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; highlight indent
(use-package col-highlight
  :config
  (column-highlight-mode 1)                ;; always highlight
  ;;(toggle-highlight-column-when-idle 1)   ;; highlight when idle
  (col-highlight-set-interval 6)
)

;; highlight colummn
(use-package crosshairs
  :config
  (crosshairs-flash 1)    ;; always enabled
)


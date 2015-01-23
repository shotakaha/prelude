;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; markdown-mode.el
;; (use-package markdown-mode
;;   :config
;;   (autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
;;   (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
;;   (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
;; )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; mardkdown-toc.el
;; ;;; Inside a markdown - M-x markdown-toc/generate-toc
;; ;;; This will compute the TOC at insert it at current position.
;; ;;; Afterwards, if a TOC is already present, it will update the one present in buffer.

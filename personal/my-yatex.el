;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; my-yatex.el --- my YaTeX config

;; ;;; add library path
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/yatex/")
;; (require 'yatex)
;; (autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
;; (setq tex-command "ptex2pdf -l -ot -synctex=1 -file-line-error")
;; (setq dviprint-command-format "dvipdfmx %s")
;; (setq dvi2-command "open -a Preview")  ;; use Preview.app
;; (setq bibtex-command "pbibtex")
;; (setq YaTeX-inhibit-prefix-letter t)
;; (setq YaTeX-skip-default-reader t)

(use-package yatex
  :ensure t
  :mode (("\\.tex$" . yatex-mode))
  :bind (("C-c C-t" . YaTeX-typeset-menu))
  :config
  ;; automatically selected according to current language
  ;; (setq YaTeX-japan t)

  ;; change default kanji-code from 2:JIS to 4:UTF-8
  ;; (setq latex-message-kanji-code 4)
  ;; (setq YaTeX-kanji-code 4)
  ;; (setq YaTeX-coding-system 4)

  ;; variables are declared in yatexlib.el
  (setq YaTeX-inhibit-prefix-letter t)
  ;; local dictionary is NOT needed
  (setq YaTeX-nervous nil)

  ;; variables are declared in yatex.el
  (setq tex-command "ptex2pdf -l -u")
  (setq bibtex-command "pbibtex")
  (setq dvi2-command "open -a Preview")
  (setq tex-pdfview-command "open -a Preview")
  (setq dviprint-command-format "dvipdfmx %s")
  (setq YaTeX-skip-default-reader t)
  (setq YaTeX-simple-messages t)
  ;; (setq YaTeX-template-file "...")
  )

;; (use-package yatex
;;   :mode (("\\.tex$" . yatex-mode))
;;   :init
;;   ;; when init : disable auto line break
;;   (add-hook 'yatex-mode-hook '(lambda ()
;;                                 (setq auto-fill-function nil)))
;;   ;; when loaded
;;   (add-hook 'yatex-mode-load-hook '(lambda ()
;;                                      (YaTeX-define-begend-key "ba" "align")
;;                                      (YaTeX-define-begend-key "bs" "subequations")))
;;   :config
;;   ;; YaTeX mode
;; (autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
;;   ;; Latex Math Preview
;;   (autoload 'latex-math-preview-expression "latex-math-preview" nil t)
;;   (autoload 'latex-math-preview-insert-symbol "latex-math-preview" nil t)
;;   (autoload 'latex-math-preview-save-image-file "latex-math-preview" nil t)
;;   (autoload 'latex-math-preview-beamer-frame "latex-math-preview" nil t)

;;   ;; typeset and convert to PDF
;;   ;; (setq tex-command "~/Library/TeXShop/bin/platex2pdf-utf8")
;;   (setq tex-command "ptex2pdf -l -ot -synctex=1 -file-line-error")
;;   (setq dviprint-command-format "dvipdfmx %s")
;;   (setq dvi2-command "open -a Preview")  ;; use Preview.app
;;   (setq bibtex-command "pbibtex")

;;   ;; Change prefix from C-c [t,c,s] --> C-c C-[t,c,s]
;;   (setq YaTeX-inhibit-prefix-letter t)
;;   ;; use AMS LaTeX
;;   ;; do not use mini-buffer when editing inside {...}
;;   (setq YaTeX-skip-default-reader t)

;;   ;; (global-font-lock-mode t)
;;   ;; (setq YaTeX-use-hilit19 t)
;;   ;; (setq YaTeX-use-font-lock t)

;; )



;; command reference for YaTeX : http://www.yatex.org/info/yatexj.html
;; [prefix] = C-c
;; [prefix] t j : typeset
;; [prefix] t r : typeset with region
;; [prefix] t k : stop typesetting
;; [prefix] t b : start jbibtex
;; [prefix] t p : preview
;; [prefix] t l : lpr (print out)
;; [prefix] t s : search with 'xdvi -remote'
;; [prefix] '   : jump to previous error occured on typeset
;;;; auto completions
;;    1. [prefix] b (or B) : \begin{xxx} ... \end{xxx}
;;    2. [prefix] s (or S) : \section{xxx}, \chapter{xxx}, etc ...
;;    3. [prefix] l (or L) : \xxx
;;    4. [prefix] m (or L) : \maketitle
;;    5. [prefix] SPC      : on demand
;;    6. [prefix] e        : \end{xxx}
;;    7. [prefix] a        : accents
;;    8. ; (math symbols)  : yatex math mode; ";->" turns into "\rightarrow"
;;    9. : (alphabet)      : yatex math mode; greek letters

;; (add-hook 'yatex-mode-hook
;;           '(lambda ()
;;              (YaTeX-define-key "p" 'latex-math-preview-expression)
;;              (YaTeX-define-key "\C-p" 'latex-math-preview-save-image-file)
;;              (YaTeX-define-key "j" 'latex-math-preview-insert-symbol)
;;              (YaTeX-define-key "\C-j" 'latex-math-preview-last-symbol-again)
;;              (YaTeX-define-key "\C-b" 'latex-math-preview-beamer-frame)))
;; (setq latex-math-preview-in-math-mode-p-func 'YaTeX-in-math-mode-p)

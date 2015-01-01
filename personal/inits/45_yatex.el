;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yatex.el

;;; add library path
(add-to-list 'load-path "~/.emacs.d/site-lisp/yatex/")

;;; YaTeX mode

;;; たぶん、下の書き方でいいはず
(add-to-list 'auto-mode-alist '("\\.tex$" . yatex-mode))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

(defvar tex-command)
(defvar dviprint-command-format)
(defvar dvi2-command)
(defvar bibtex-command)
(defvar YaTeX-inhibit-prefix-letter)
(defvar YaTeX-use-AMS-LaTeX)
(defvar YaTeX-skip-default-reader)

;;(setq tex-command "platex")

;;(setq tex-command "~/Library/TeXShop/bin/platex2pdf-utf8") ;; typeset and convert to PDF
(setq tex-command "ptex2pdf -l -ot -synctex=1 -file-line-error")
(setq dviprint-command-format "dvipdfmx %s")
(setq dvi2-command "open -a Preview")  ;; use Preview.app
(setq bibtex-command "pbibtex")

(setq YaTeX-inhibit-prefix-letter t)   ;; prefix changed C-c --> C-c C-[t, c, s]

;; (global-font-lock-mode t)
;; (setq YaTeX-use-hilit19 t)
;; (setq YaTeX-use-font-lock t)
(add-hook 'yatex-mode-hook   ;; when init
          '(lambda ()
             (setq auto-fill-function nil))) ;; disable auto line break
(add-hook 'yatex-mode-load-hook  ;; when loaded
          '(lambda ()
             (YaTeX-define-begend-key "ba" "align")
             (YaTeX-define-begend-key "bs" "subequations")))
(setq YaTeX-use-AMS-LaTeX t)  ;; use AMS LaTeX
(setq YaTeX-skip-default-reader t)  ;; do not use mini-buffer when editing inside {...}

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Latex Math Preview
(autoload 'latex-math-preview-expression "latex-math-preview" nil t)
(autoload 'latex-math-preview-insert-symbol "latex-math-preview" nil t)
(autoload 'latex-math-preview-save-image-file "latex-math-preview" nil t)
(autoload 'latex-math-preview-beamer-frame "latex-math-preview" nil t)

;; (add-hook 'yatex-mode-hook
;;           '(lambda ()
;;              (YaTeX-define-key "p" 'latex-math-preview-expression)
;;              (YaTeX-define-key "\C-p" 'latex-math-preview-save-image-file)
;;              (YaTeX-define-key "j" 'latex-math-preview-insert-symbol)
;;              (YaTeX-define-key "\C-j" 'latex-math-preview-last-symbol-again)
;;              (YaTeX-define-key "\C-b" 'latex-math-preview-beamer-frame)))
;; (setq latex-math-preview-in-math-mode-p-func 'YaTeX-in-math-mode-p)

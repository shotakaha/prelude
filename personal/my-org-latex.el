;;; my-org-latex.el --- config for org-latex export

;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setting LaTeX for Org
;;; Org -> LaTeX file -> YaTeX(pTeX + dvipdfmx)
;;; 1. デフォルトクラスを jsarticle に設定（org-latex-classes）
;;; 2. パッケージの追加（org-latex-packages-list）
;;; 3. コードブロックの装飾（org-latex-listings）
;;; 4. 上付き文字、下付き文字の設定(org-use-sub-superscripts)
;;; 5. org-latex-pdf-process の設定をする予定
;;; 6. 未確認な設定をコメントアウト

;;; Code:

(use-package ox-latex
  :config
  (setq org-latex-default-class "jsarticle")
  (add-to-list 'org-latex-classes
               '("jsarticle"
                 "\\documentclass[dvipdfmx,12pt]{jsarticle}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
                 )
               )
  (add-to-list 'org-latex-classes
               '("jsreport"
                 "\\documentclass[dvipdfmx,12pt,report]{jsbook}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 )
               )
  (add-to-list 'org-latex-classes
               '("jsbook"
                 "\\documentclass[dvipdfmx,12pt]{jsbook}"
                 ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 )
               )
  (add-to-list 'org-latex-classes
               '("bxjsarticle"
                 "\\documentclass[pdflatex,jadriver=standard,12pt]{bxjsarticle}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
                 )
               )
  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass[dvipdfmx,12pt]{beamer}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
                 )
               )

  ;; hyperrefのしおりの文字化け対策
  (add-to-list 'org-latex-packages-alist '("" "pxjahyper") t)
  ;; (add-to-list 'org-latex-packages-alist '("" "atbegshi") t)
  ;; (add-to-list 'org-latex-packages-alist "\\AtBeginShipoutFirst{\\special{pdf:tounicode EUC-UCS2}}" t)

  ;; hyperrefの設定
  (add-to-list 'org-latex-packages-alist "\\hypersetup{setpagesize=false}" t)
  (add-to-list 'org-latex-packages-alist "\\hypersetup{colorlinks=true}" t)
  (add-to-list 'org-latex-packages-alist "\\hypersetup{linkcolor=blue}" t)

  (add-to-list 'org-latex-packages-alist '("" "listings") t)
  (add-to-list 'org-latex-packages-alist '("" "color") t)
  (add-to-list 'org-latex-packages-alist '("" "fancyvrb") t)

  ;;(add-to-list 'org-latex-packages-alist '("" "minted")) ;; gives an error while compilling
  (setq org-latex-listings t)

  (setq org-use-sub-superscripts nil)
  (setq org-export-with-sub-superscripts nil)

  ;; (setq org-latex-pdf-process
  ;;       ("ptex2pdf -l -ot -synctex=1 -file-line-error")

  ;;       )
  ;; (setq org-export-latex-coding-system "utf8")
  ;; (setq org-export-latex-date-format "%Y-%m-%d")
  ;; (setq org-file-apps '(("pdf" . "/usr/bin/open -a Preview.app %s")))

  )

(provide 'my-org-latex)
;;; my-org-latex.el ends here

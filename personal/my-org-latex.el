;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setting LaTeX for Org
;;; Org -> LaTeX file -> YaTeX(pTeX + dvipdfmx)
;;; 1. デフォルトクラスを jsarticle に設定（org-latex-classes）
;;; 2. パッケージの追加（org-latex-packages-list）
;;; 3. コードブロックの装飾（org-latex-listings）
;;; 4. org-latex-pdf-process の設定をする予定
;;; 5. 未確認な設定をコメントアウト

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
                 ))

  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("" "fancyvrb"))
  ;;(add-to-list 'org-latex-packages-alist '("" "minted")) ;; gives an error while compilling
  (setq org-latex-listings nil)

  ;; (setq org-latex-pdf-process
  ;;       ("ptex2pdf -l -ot -synctex=1 -file-line-error")
  ;;       )

  ;; (setq org-export-latex-coding-system "utf8")
  ;; (setq org-export-latex-date-format "%Y-%m-%d")
  ;; (setq org-file-apps '(("pdf" . "/usr/bin/open -a Preview.app %s")))

  )

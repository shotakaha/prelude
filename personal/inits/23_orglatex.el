;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setting LaTeX for Org
;;; Org -> LaTeX file -> YaTeX(pTeX + dvipdfmx)
(require 'ox-latex)
(setq org-export-latex-coding-system "utf8")
(setq org-export-latex-date-format "%Y-%m-%d")
(setq org-latex-default-class "jsarticle")
;;; (setq org-latex-pdf-process '("/usr/texbin/latexmk -e '$latex=q/uplatex %S/' -e $bibtex=q/upbibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/mendex -U -o %D %S/' -e '$dvipdf=q/dvipdfmx -o %D %S/' -norc -gg -pdfdvi %f"))
;; (setq org-latex-pdf-process
;;       '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(setq org-file-apps '(("pdf" . "/usr/bin/open -a Preview.app %s")))
;; (add-to-list 'org-latex-classes
;;              '("jsarticle" "\\documentclass[dvipdfmx,12pt]{jsarticle}"
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
;;                ))


;; (add-to-list 'org-latex-packages-alist '("" "color"))
;; (add-to-list 'org-latex-packages-alist '("" "listings"))
;;(add-to-list 'org-latex-packages-alist '("" "minted")) ;; gives an error while compiling
;;(setq org-latex-listings 'minted)  ;; gives an error while exporting

(add-to-list 'org-latex-classes
             '("jsarticle" "\\documentclass[dvipdfmx,12pt]{jsarticle}
[NO-DEFAULT-PACKAGES]
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath, amssymb}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{hyperref}
\\hypersetup{setpagesize=false,
colorlinks=true,
linkcolor=blue}
\\tolerance=1000
"
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")
("\\subparagraph{%s}" . "\\subparagraph*{%s}")
))

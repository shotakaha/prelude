;;; my-org.el

;;; Commentary:
;;
;; ある程度は、modules/prelude-org.el でも設定されている。
;; このファイルでやっていることは、
;;   1. org-capture の設定
;;   2. org-agenda の設定（まだ終わっていない）
;;   3. org-gcal の設定 -> 外部ファイルへ
;;   4. org-bpe の設定 -> 外部ファイルへ
;;   5. org-ac の設定
;;   6. org-autolistの設定
;;   7. org-doingの設定
;;
;; org-capture などで作成するファイルは、
;; 基本的に ~/Documents/org/ に保存することにする。

;;; Code:

;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ;;; Configure before loading org mode
;; ;; (package-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-mode --- M-x org-info でマニュアルを参照可能
(use-package org
  :ensure t
  :mode (("\\.txt$" . org-mode))
  :config

  ;; 保存先（もっとうまく書けたらいいのになぁ）
  (setq my-org-directory "~/Documents/org/")
  (setq my-org-capture-demo-file "~/Documents/org/captured-demo.org")
  (setq my-org-codereading-file "~/Documents/org/codereading.org")
  (setq my-org-agenda-directory "~/Documents/org/agenda/")
  ;;(setq my-org-agenda-directory "~/Documents/org/")

  ;; 基本設定
  ;; Hide the first N-1 stars in a headline : nil --> t
  (setq org-hide-leading-stars t)
  ;; RET will follow the link : nil --> t
  (setq org-return-follows-link t)
  ;; Directory with org files : "~/org" --> "~/Documents/org"
  (setq org-directory my-org-directory)
  ;; Default target for storing notes : "~/.notes" --> "captured.org"
  (setq org-default-notes-file "captured.org")

  ;; org-capture
  (setq org-capture-templates
        `(("a" "あっと思ったことを さっとφ(..)メモする" entry
           (file+headline nil "MEMO")
           "* %U%?\n\n%a\n%F\n"
           :empty-lines 1)

          ("b" "ブログのネタなど" entry
           (file+headline nil "BLOG")
           "* %?\n   Entered on %U"
           :empty-lines 1
           :jump-to-captured 1)

          ("m" "みんなで会議" entry
           (file+datetree "~/Documents/org/minutes.org")
           "* %? %T"
           :empty-lines 1
           :jump-to-captured 1)

          ("p" "ぱっと 読み返したいと思ったとき" plain
           (file+headline nil "PLAIN")
           "%?"
           :empty-lines 1
           :jump-to-captured 1
           :unnarrowed 1)

          ("t" "とりあえず 仕事を放り込む" entry
           (file+headline nil "GTD")
           "** TODO %T %?\n   Entered on %U    %i\n"
           :empty-lines 1
           :unnarrowd 1)
          )
        )

  (bind-key "C-x C-z" 'org-capture-demo)
  ;; (bind-key "C-x C-x" 'org-capture-code-reading)
  (bind-key "C-c c" 'org-capture)


  ;; org-agenda の設定

  ;; todoキーワードの設定 --- tamura70ブログ
  ;; http://d.hatena.ne.jp/tamura70/20100207/org
  ;; http://d.hatena.ne.jp/tamura70/20100215/org

  ;; TODOの状態の設定
  ;; ! をつけることで、その状態へ変更した日時を記録することが可能
  ;; @ をつけることで、その状態へ変更した時にメモを残すことが可能
  (setq org-todo-keywords
        '((sequence "APPT(a@/!)" "TODO(t)" "STARTED(s!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCEL(c@/!)")))
  (setq org-log-done 'time)   ;;; DONEの時刻を記録
  ;; (setq org-log-done 'note)  ;;; DONEの時刻とメモを記録

  ;; tagリストの設定 --- tamura70のブログ
  ;; http://d.hatena.ne.jp/tamura70/20100215/org
  ;; TAGリストの一括設定
  ;; orgファイル毎で設定する場合は，ファイル中に以下のように記述する．
  ;; #+TAGS: @OFFICE(o) @HOME(h)
  ;; #+TAGS: SHOPPING(s) MAIL(m) PROJECT(p)
  (setq org-tag-alist
        '(("@OFFICE" . ?o) ("@HOME" . ?h)
          ("MAIL" . ?m) ("WRITE" . ?w)
          ("ASK" . ?a)))

  ;; ;; AGENDAの設定 --- tamura70ブログ
  ;; ;; http://d.hatena.ne.jp/tamura70/20100208/org
  ;; ;; アジェンダ表示の対象ファイル
  ;; (setq org-agenda-files (list my-org-agenda-directory))
  ;; (setq org-agenda-files (list my-org-directory))
  (setq org-agenda-files (list my-org-directory my-org-agenda-directory))
  ;; アジェンダ表示で下線を用いる
  (add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
  ;; (setq hl-line-face 'underline)
  ;; 標準の祝日を利用しない
  (setq calendar-holidays nil)



;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ;;; これ以下の設定は、たぶんるびきちさんの本を見たんだと思う
;; ;; ;;; その本が手元にないので、とりあえずコメントあうとしておく
;; ;; ;; (defun org-sert-upheading (arg)
;; ;; ;;   "Insert upper level headings"
;; ;; ;;   (interactive "P")
;; ;; ;;   (org-insert-heading arg)
;; ;; ;;   (cond ((org-on-heading-p) (org-do-promote))
;; ;; ;;         ((org-at-item-p) (org-indent-item))))
;; ;; ;; (defun org-insert-heading-dwim (arg)
;; ;; ;;   "Insert the same level headings. C-u: upper level, C-u C-u: lower level"
;; ;; ;;   (interactive "p")
;; ;;   (case arg
;; ;; ;;     (4  (org-insert-subheading nil))  ; C-u
;; ;; ;;     (16 (org-insert-upheading  nil))  ; C-u C-u
;; ;; ;;     (t  (org-insert-heading nil))))
;; ;; ;; (define-key org-mode-map (kbd "<C-return>") 'org-insert-heading-dwim)

;; ;; ;; ;;; Setting for todo-template
;; ;; ;; (setq org-use-fast-todo-selection t)
;; ;; ;; (setq org-todo-keywords
;; ;; ;;       '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(x)" "CANCEL(c)")
;; ;; ;;         (sequence "APPT(a)" "|" "DONE(x)" "CANCEL(c)")))
;;   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-ac
;;; https://github.com/aki2o/org-ac

(use-package org-ac
  :config
  ;; Make config suit for you. About the config item, eval the following sexp.
  ;; (customize-group "org-ac")
  (org-ac/config-default)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-autolist
;;; https://github.com/calvinwyoung/org-autolist
(use-package org-autolist
  :config
  (add-hook 'org-mode-hook (lambda () (org-autolist-mode)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-doing
;;; http://rubikitch.com/2015/06/26/org-doing/
(use-package org-doing
  :config
  (setq org-doing-file "~/Documents/org/doing.txt")
  )

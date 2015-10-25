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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-mode --- M-x org-info でマニュアルを参照可能
(use-package org
  :ensure t
  :mode (("\\.txt$" . org-mode))
  :bind (("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         )
  :init
  (setq my-org-directory "~/Documents/org/")
  (setq my-org-agenda-directory "~/Documents/org/agenda/")
  (setq my-org-default-notes-file "captured.org")
  :config
  (setq org-directory my-org-directory)
  (setq org-default-notes-file my-org-default-notes-file)
  (setq org-agenda-files (list my-org-directory my-org-agenda-directory))

  ;; 基本設定
  ;; Hide the first N-1 stars in a headline : nil --> t
  (setq org-hide-leading-stars t)
  ;; RET will follow the link : nil --> t
  (setq org-return-follows-link t)
  ;; Directory with org files : "~/org" --> "~/Documents/org"

  ;; Default target for storing notes : "~/.notes" --> "captured.org"


  ;; org-capture
  (setq org-capture-templates
        `(("a" "あっと思ったことを さっとφ(..)メモする" entry
           (file+headline nil "MEMO")
           "* %U%?\n\n%a\n%F\n"
           :empty-lines 1)

          ("b" "ブログのネタなど" entry
           (file+headline nil "BLOG")
           "* %T %?\n   Entered on %U"
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

          ("s" "写真の処理状況ログ" entry
           (file+datetree "~/Documents/org/photolog.org")
           "** TODO %T %?\n   Entered on %U    %i\n"
           :empty-lines 1
           :unnarrowed 1)

          ("t" "とりあえず 仕事を放り込む" entry
           (file+headline "~/Documents/org/gtd.org" "GTD")
           "** TODO %?\n   SCHEDULED: %T\n   Entered on %U    %i\n"
           :prepend 1
           :empty-lines 1
           :unnarrowed 1)
          )
        )

  ;; org-agenda の設定
  ;; 標準の祝日を利用しない
  (setq calendar-holidays nil)

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

  ;; アジェンダ表示で下線を用いる
  (add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-ac
;;; https://github.com/aki2o/org-ac

(use-package org-ac
  :disabled t
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ox-rst
(use-package ox-rst :ensure t)

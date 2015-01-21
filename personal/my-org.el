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
;;
;; org-capture などで作成するファイルは、
;; 基本的に ~/Documents/org/ に保存することにする。

;;; Code:

;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ;;; Configure before loading org mode
;; ;; (package-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-mode --- M-x org-info でマニュアルを参照可能
(prelude-require-package 'org)
(use-package org
  :mode (("\\.txt$" . org-mode))
  :config

  ;; 保存先（もっとうまく書けたらいいのになぁ）
  (setq my-org-directory "~/Documents/org/")
  (setq my-org-capture-demo-file "~/Documents/org/captured-demo.org")
  (setq my-org-codereading-file "~/Documents/org/codereading.org")
  (setq my-org-agenda-directory "~/Documents/org/agenda/")
  ;;(setq my-org-agenda-directory "~/Documents/org/")

  ;; 基本設定
  ;; 見出しの余分な * を消す。意外と見やすくなる。
  (setq org-hide-leading-stars t)
  ;; これはなんだろう？
  ;; (setq org-startup-truncated nil)
  ;; RET でリンク先へ移動する
  (setq org-return-follows-link t)
  ;; 作成したファイルの保存先
  (setq org-directory my-org-directory)
  ;; デフォルトのファイル名
  (setq org-default-notes-file "captured.org")

  ;; org-capture の設定
  ;; 1. org-capture-demo
  ;;    http://d.hatena.ne.jp/rubikitch/20100819/org
  ;;    demoを作成し、改変していく
  ;;    保存先は my-org-capture-demo-file にする
  ;; 2. org-capture-code-reading
  ;;    http://d.hatena.ne.jp/rubikitch/20090121/1232468026
  ;;    org-remember -> org-capture に置き換える

  ;; demoをイジったり、しらべたりしてわかったこと:
  ;;

  ;; org-capture-templates の引数 => org-capture.el を読むのが一番
  ;; <file:~/.emacs.d/elpa/org-20150105/org-capture.el>
  ;; 1. key
  ;;    ショートカットキー
  ;; 2. description
  ;;    org-capture したときに表示されるメッセージ
  ;; 3. type {entry, item, checkitem, table-line, plain}
  ;;    入力内容の種類
  ;;    とりあえず、entryにしとけばよい
  ;; 4. target : default = org-default-notes-file
  ;;    - 使いそうなものを挙げておく
  ;;      - (file "path-to-file")
  ;;      - (id "id-of-existing-entry")
  ;;      - (file+headline "path-to-file" "node-headline")
  ;;      - (file+datetree "path-to-file")
  ;;      - (file+datetree+prompt "path-to-file")
  ;;      - (clock)
  ;; 5. template
  ;;    - org-capture した時に自動で挿入される文章
  ;;      - (file "path-to-template-file")
  ;;      - テンプレートで使えるエスケープ文字
  ;;      - %t, %T, %u, %U = 日時（日付のみ、日付＋時刻）（inactive な日時）
  ;;      - %a = Annotation (default:org-store-link)
  ;;      - %n = username (user-full-name)
  ;;      - %f, %F = org-capture した場所のファイル名（フルパス）
  ;;      - %:keyword = 便利なテンプレがありそうだが、使い方がよくわからない
  ;;      - %? = テンプレ挿入後のカーソルの位置
  ;; 6以降 : property
  ;;    - テンプレートの挿入位置、など
  ;;      - :prepend
  ;;      - :immediate-finish = テンプレートをちゃんと用意しておけば、ファイルのブックマークを作れそう
  ;;      - :jump-to-captured = 編集後、capture したファイルを開く
  ;;      - :empty-lines, empty-lines-before, :empty-lines-after = エントリーの前後にいれる空行の数を指定する
  ;;      - :clock-in, clock-keep, clock-resume = org-capture したときに、clockをどうするか
  ;;      - :unnarrowed = org-capture した時に全表示する（デフォルトは、追記部分のみ表示）

  (defun org-capture-demo ()
    (interactive)
    (let ((file my-org-capture-demo-file)
          org-capture-templates)
      (find-file-other-window file)
      (unless (save-excursion
                (goto-char 1)
                (search-forward "#+title: org-capture-demo\n* entry\n" nil t))
        (insert "#+title: org-capture-demo\n* entry\n"))
      (other-window 1)

      ;; (setq org-capture-templates
      ;;       `(("a" "ふつうのエントリー後に追加" entry (file+headline ,file "entry") "* %?\n%U\n%a\n%f\n")
      ;;         ("b" "ふつうのエントリー前に追加" entry (file+headline ,file "entry") "* %?\n%U\n%a\n" :prepend t)
      ;;         ("c" "即座に書き込み" entry (file+headline ,file "entry") "* immediate-finish\n" :immediate-finish t)
      ;;         ("d" "ナローイングしない" entry (file+headline ,file "entry") "* 全体を見る\n\n" :unnarrowed t)
      ;;         ("e" "クロック中のエントリに追加" entry (clock) "* clocking" :unnarrowed t)
      ;;         ("f" "リスト" item (file+headline ,file "list") "- リスト")
      ;;         ;; うまく動かない
      ;;         ("g" "チェックリスト" checkitem (file+headline ,file "list") "チェックリスト")
      ;;         ("h" "表の行" table-line (file+headline ,file "table") "|表|")
      ;;         ("i" "そのまま" plain (file+headline ,file "plain") "あいうえお")
      ;;         ("j" "ノードをフルパス指定して挿入" entry (file+olp ,file "test" "entry") "* %?\n%U\n%a\n")
      ;;         ("k" "ノードを正規表現指定して挿入" entry (file+regexp ,file "list") "* %?\n%U\n%a\n")
      ;;         ;; 年月日エントリは追記される
      ;;         ("l" "年/月/日のエントリを作成する1" entry (file+datetree ,file))
      ;;         ("m" "年/月/日のエントリを作成する2" item  (file+datetree ,file))
      ;;         ("o" "年/月/日のエントリを作成する prepend" entry (file+datetree ,file) "* a" :prepend t))
      ;;       )
      (org-capture)
      ))

  (setq org-capture-templates
        `(("a" "あっと思ったことを さっとφ(..)メモする" entry
           (file+headline nil "MEMO")
           "* %U%?\n\n%a\n%F\n"
           :empty-lines 1)

          ("b" "ブログのネタなど" entry
           (file+headline nil "BLOG")
           "* %?\nEntered on %U"
           :empty-lines 1
           :jump-to-captured 1)

          ("m" "みんなで会議" entry
           (file+datetree "~/Documents/org/minutes.org")
           "* %T %?"
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
           "** TODO %?\n    %i\n    %a\n    Enterd on %U"
           :empty-lines 1)
          )
        )


  ;; ;; org-capture-templates 変数の設定
  ;; (setq org-capture-templates

  ;;         ("b" "Bug"  entry (file+headline nil "Inbox")     "** TODO %?    :bug:\n    %i\n    %a\n    %t")
  ;;         ("i" "Idea" entry (file+headline nil "New Ideas") "** %?\n    %i\n    %a\n    %t")
  ;;
  ;;         ("d" "Dict" entry (file+headline nil "Dict")      "** %?\n    %i\n    %a\n    %t")
  ;;         )
  ;;       '(("Note" ?n "** %?\n    %i\n    %a\n    %t"       nil "Inbox")
  ;;         ("Todo" ?t "** ToDo: %?\n    %i\n    %a\n    %t" nil "Inbox")
  ;;         ("Log"  ?l "** Log: %?\n    %i\n    %a\n    %t"  nil "Inbox")
  ;;         )
  ;;       )



  ;; ひき続きるびきちブログに書いてあったコードリーディングの設定
  ;; http://d.hatena.ne.jp/rubikitch/20090121/1232468026
  ;; ~/Documents/org/code-reading.org に記録する
  ;; org-remember -> org-capture に置き換え
  (defvar org-code-reading-software-name nil)
  (defvar org-code-reading-file "code-reading.org")
  (defun org-code-reading-read-software-name ()
    (set (make-local-variable 'org-code-reading-software-name)
         (read-string "Code Reading Software: "
                      (or org-code-reading-software-name
                          (file-name-nondirectory
                           (buffer-file-name))))))

  (defun org-code-reading-get-prefix (lang)
    (concat "[" lang "]"
            "[" (org-code-reading-read-software-name) "]"))

  (defun org-capture-code-reading ()
    (interactive)
    (let* ((prefix (org-code-reading-get-prefix (substring (symbol-name major-mode) 0 -5)))
           (org-capture-templates
            `(("CodeReading" ?r "** %(identity prefix)%?\n   \n   %a\n   %t", org-code-reading-file "Memo"))))
      (org-capture)))

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
  (setq org-agenda-files (list my-org-agenda-directory))
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
(require 'org-ac)
;; Make config suit for you. About the config item, eval the following sexp.
;; (customize-group "org-ac")
(org-ac/config-default)

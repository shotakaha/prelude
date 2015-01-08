;;; my-org.el

;;; Commentary:
;;
;; Configured in modules/prelude-org.el
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
  (setq my-org-agenda-directory "~/Documents/org/agenda/")
  (setq my-org-capture-file "~/Documents/org/captured-demo.org")
  (setq my-org-codereading-file "~/Documents/org/codereading.org")

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

  ;; TODOの設定 --- tamura70ブログ
  ;; http://d.hatena.ne.jp/tamura70/20100207/org
  ;; http://d.hatena.ne.jp/tamura70/20100215/org
  ;; TODOの状態の設定
  ;; ! をつけることで、その状態へ変更した日時を記録することが可能
  ;; @ をつけることで、その状態へ変更した時にメモを残すことが可能
  (setq org-todo-keywords
        '((sequence "APPT(a)" "TODO(t)" "STARTED(s!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCEL(c@/!)")))
  (setq org-log-done 'time)   ;;; DONEの時刻を記録
  ;; (setq org-log-done 'note)  ;;; DONEの時刻とメモを記録

  ;; TAGの設定 --- tamura70のブログ
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

  ;; org-capture の設定
  ;; http://d.hatena.ne.jp/rubikitch/20090121/1232468026
  ;; まずはデモを試してみる
  ;; C-x C-z に割り当ててある
  ;; 保存先は my-org-capture-file にした
  ;; http://d.hatena.ne.jp/rubikitch/20100819/org
  ;; setq org-capture-templates の部分は外に抜き出した。

  (defun org-capture-demo ()
    (interactive)
    (let ((file my-org-capture-file)
          org-capture-templates)
      (find-file-other-window file)
      (unless (save-excursion
                (goto-char 1)
                (search-forward "#+title: org-capture-demo\n* entry\n" nil t))
        (insert "#+title: org-capture-demo\n* entry\n"))
      (other-window 1)

      (setq org-capture-templates
            `(("a" "ふつうのエントリー後に追加" entry (file+headline ,file "entry") "* %?\n%U\n%a\n")
              ("b" "ふつうのエントリー前に追加" entry (file+headline ,file "entry") "* %?\n%U\n%a\n" :prepend t)
              ("c" "即座に書き込み" entry (file+headline ,file "entry") "* immediate-finish\n" :immediate-finish t)
              ("d" "ナローイングしない" entry (file+headline ,file "entry") "* 全体を見る\n\n" :unnarrowed t)
              ("e" "クロック中のエントリに追加" entry (clock) "* clocking" :unnarrowed t)
              ("f" "リスト" item (file+headline ,file "list") "- リスト")
              ;; うまく動かない
              ("g" "チェックリスト" checkitem (file+headline ,file "list") "チェックリスト")
              ("h" "表の行" table-line (file+headline ,file "table") "|表|")
              ("i" "そのまま" plain (file+headline ,file "plain") "あいうえお")
              ("j" "ノードをフルパス指定して挿入" entry (file+olp ,file "test" "entry") "* %?\n%U\n%a\n")
              ("k" "ノードを正規表現指定して挿入" entry (file+regexp ,file "list") "* %?\n%U\n%a\n")
              ;; 年月日エントリは追記される
              ("l" "年/月/日のエントリを作成する1" entry (file+datetree ,file))
              ("m" "年/月/日のエントリを作成する2" item  (file+datetree ,file))
              ("o" "年/月/日のエントリを作成する prepend" entry (file+datetree ,file) "* a" :prepend t))
            )
      (org-capture)
      ))

  ;; ;; org-capture-templates 変数の設定
  ;; (setq org-capture-templates
  ;;       '(("t" "Todo" entry (file+headline nil "Inbox")     "** TODO %?\n    %i\n    %a\n    %t")
  ;;         ("b" "Bug"  entry (file+headline nil "Inbox")     "** TODO %?    :bug:\n    %i\n    %a\n    %t")
  ;;         ("i" "Idea" entry (file+headline nil "New Ideas") "** %?\n    %i\n    %a\n    %t")
  ;;         ("l" "Log"  entry (file+headline nil "Log")       "** %?\n    %i\n    %a\n    %t")
  ;;         ("d" "Dict" entry (file+headline nil "Dict")      "** %?\n    %i\n    %a\n    %t")
  ;;         )
  ;;       '(("Note" ?n "** %?\n    %i\n    %a\n    %t"       nil "Inbox")
  ;;         ("Todo" ?t "** ToDo: %?\n    %i\n    %a\n    %t" nil "Inbox")
  ;;         ("Log"  ?l "** Log: %?\n    %i\n    %a\n    %t"  nil "Inbox")
  ;;         )
  ;;       )

  ;; ;; capture templates
  ;; (setq org-capture-templates
  ;;       '(("p" "Project Task" entry (file+headline (expand-file-name "~/Documents/org/project.org") "Inbox") "** TODO %?\n    %i\n    %a\n    %T")
  ;;         ("m" "memo"         entry (file (expand-file-name "~/Documents/org/memo.org")) "* %?\n    %i\n    %a\n    %T"))
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



  ;; ひき続きるびきちブログに書いてあった、リンク間移動の設定
  ;; るびきちブログによると
  ;; 『「C-c C-x C-p」と「C-c C-x C-n」*2を押すとブラウザで
  ;;   Tabを押すようにリンクからリンクの移動ができるのだが、
  ;;   そのままだと隠れているリンクに到達してしまうのでいまいち不便だ。
  ;;   見えている部分のみのリンク間移動のコマンドを定義する。
  ;;   「M-p」と「M-n」に割り当てておいた。
  ;;   これで検索結果から快適にリンク先に飛べるぞ』
  ;; 正直、あまりorgを使いこなせていないので、
  ;; リンク間移動をしたことないんだけど、便利そうなので設定しておく

  ;; 次の見えているリンクへ移動
  (defun org-next-visible-link ()
    "Move forward to the next link.
  If the link is in hidden text, expose it."
    (interactive)
    (when (and org-link-search-failed (eq this-command last-command))
      (goto-char (point-min))
      (message "Link search wrapped back to beginning of buffer"))
    (setq org-link-search-failed nil)
    (let* ((pos (point))
           (ct (org-context))
           (a (assoc :link ct))
           srch)
      (if a (goto-char (nth 2 a)))
      (while (and (setq srch (re-search-forward org-any-link-re nil t))
                  (goto-char (match-beginning 0))
                  (prog1 (not (eq (org-invisible-p) 'org-link))
                    (goto-char (match-end 0)))))
      (if srch
          (goto-char (match-beginning 0))
        (goto-char pos)
        (setq org-link-search-failed t)
        (error "No further link found"))))

  ;; 前の見えているリンクへ移動
  (defun org-previous-visible-link ()
    "Move backward to the previous link.
  If the link is in hidden text, expose it."
    (interactive)
    (when (and org-link-search-failed (eq this-command last-command))
      (goto-char (point-max))
      (message "Link search wrapped back to end of buffer"))
    (setq org-link-search-failed nil)
    (let* ((pos (point))
           (ct (org-context))
           (a (assoc :link ct))
           srch)
      (if a (goto-char (nth 1 a)))
      (while (and (setq srch (re-search-backward org-any-link-re nil t))
                  (goto-char (match-beginning 0))
                  (not (eq (org-invisible-p) 'org-link))))
      (if srch
          (goto-char (match-beginning 0))
        (goto-char pos)
        (setq org-link-search-failed t)
        (error "No further link found"))))

  (bind-keys :map org-mode-map
             ("M-n" 'org-next-visible-link)
             ("M-p" 'org-previous-visible-link)
             )
  )


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

;;; my-org.el

;;; Commentary:
;;
;; Configured in modules/prelude-org.el
;;

;;; Code:

;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ;;; Configure before loading org mode
;; ;; (package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-mode --- M-x org-info でマニュアルを参照可能
(use-package org
  :mode (
         ("\\.txt$" . org-mode)
         )
  :config
  (progn
    ;; 見出しの余分な * を消す。意外と見やすくなる。
    (setq org-hide-leading-stars t)
    )
  )


;; ;;; tamura70さんのはてなブログも参照
;; ;;; http://d.hatena.ne.jp/tamura70/20100203/org
;; ;;; 拡張子が org のファイルを開いたとき、自動的に org-mode にする
;; ;;; 拡張子が txt のファイルも org-mode にする
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; (add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))

;; ;;; org-mode での強調表示を可能にする
;; (add-hook 'org-mode-hook 'turn-on-font-lock)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; org-remember の設定 --- るびきちブログ
;; ;;; http://d.hatena.ne.jp/rubikitch/20090121/1232468026
;; ;;; org-modeがうまく動作しない（特にorg-agenda）ので
;; ;;; テストとして、~/Documents/org/ にいろいろ作ることにする
;; ;;; --> ~/Documents/junk/ に変更した
;; ;;; --> ~/Documents/org/ に戻した。C-c a t でTODO リストを表示する際
;; ;;;     junk 以下のファイルをすべてバッファに取り込むせいか、
;; ;;;     emacs がすごく重たくなるため
;; ;;;
;; ;;; org-capture の設定 --- るびきちブログ
;; ;;; http://d.hatena.ne.jp/rubikitch/20100819/org
;; ;;; org-remember は org-capture になったみたい
;; (setq org-startup-truncated nil)
;; (setq org-return-follows-link t)
;; (setq org-directory "~/Documents/org/")
;; (setq org-default-notes-file "agenda.org")
;; (setq org-capture-templates
;;       '(("t" "Todo" entry
;;          (file+headline nil "Inbox")
;;          "** TODO %?\n    %i\n    %a\n    %t")
;;         ("b" "Bug" entry
;;          (file+headline nil "Inbox")
;;          "** TODO %?    :bug:\n    %i\n    %a\n    %t")
;;         ("i" "Idea" entry
;;          (file+headline nil "New Ideas")
;;          "** %?\n    %i\n    %a\n    %t")
;;         ("l" "Log" entry
;;          (file+headline nil "Log")
;;          "** %?\n    %i\n    %a\n    %t")
;;         ("d" "Dict" entry
;;          (file+headline nil "Dict")
;;          "** %?\n    %i\n    %a\n    %t")
;;         ))
;;       ;; '( ("Note" ?n "** %?\n    %i\n    %a\n    %t" nil "Inbox")
;;       ;;    ("Todo" ?t "** ToDo: %?\n    %i\n    %a\n    %t" nil "Inbox")
;;       ;;    ("Log"  ?l "** Log: %?\n    %i\n    %a\n    %t" nil "Inbox")
;;       ;;    ) )

;; ;; ;; ;;; capture templates
;; ;; ;; (setq org-capture-templates
;; ;; ;;       '(("p" "Project Task" entry (file+headline (expand-file-name "~/Documents/org/project.org") "Inbox")
;; ;; ;;          "** TODO %?\n    %i\n    %a\n    %T")
;; ;; ;;         ("m" "memo" entry (file (expand-file-name "~/Documents/org/memo.org"))
;; ;; ;;          "* %?\n    %i\n    %a\n    %T")))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; ひき続きるびきちブログに書いてあったコードリーディングの設定
;; (defvar org-code-reading-software-name nil)
;; ;; ~/Documents/org/code-reading.org に記録する
;; (defvar org-code-reading-file "code-reading.org")
;; (defun org-code-reading-read-software-name ()
;;   (set (make-local-variable 'org-code-reading-software-name)
;;        (read-string "Code Reading Software: "
;;                     (or org-code-reading-software-name
;;                         (file-name-nondirectory
;;                          (buffer-file-name))))))

;; (defun org-code-reading-get-prefix (lang)
;;   (concat "[" lang "]"
;;           "[" (org-code-reading-read-software-name) "]"))
;; (defun org-remember-code-reading ()
;;   (interactive)
;;   (let* ((prefix (org-code-reading-get-prefix (substring (symbol-name major-mode) 0 -5)))
;;          (org-remember-templates
;;           `(("CodeReading" ?r "** %(identity prefix)%?\n   \n   %a\n   %t"
;;              ,org-code-reading-file "Memo"))))
;;     (org-remember)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; ひき続きるびきちブログに書いてあった、リンク間移動の設定
;; ;;; るびきちブログによると
;; ;;; 『「C-c C-x C-p」と「C-c C-x C-n」*2を押すとブラウザで
;; ;;;   Tabを押すようにリンクからリンクの移動ができるのだが、
;; ;;;   そのままだと隠れているリンクに到達してしまうのでいまいち不便だ。
;; ;;;   見えている部分のみのリンク間移動のコマンドを定義する。
;; ;;;   「M-p」と「M-n」に割り当てておいた。
;; ;;;   これで検索結果から快適にリンク先に飛べるぞ』
;; ;;; 正直、あまりorgを使いこなせていないので、
;; ;;; リンク間移動をしたことないんだけど、便利そうなので設定しておく

;; (defun org-next-visible-link ()
;;   "Move forward to the next link.
;; If the link is in hidden text, expose it."
;;   (interactive)
;;   (when (and org-link-search-failed (eq this-command last-command))
;;     (goto-char (point-min))
;;     (message "Link search wrapped back to beginning of buffer"))
;;   (setq org-link-search-failed nil)
;;   (let* ((pos (point))
;;          (ct (org-context))
;;          (a (assoc :link ct))
;;          srch)
;;     (if a (goto-char (nth 2 a)))
;;     (while (and (setq srch (re-search-forward org-any-link-re nil t))
;;                 (goto-char (match-beginning 0))
;;                 (prog1 (not (eq (org-invisible-p) 'org-link))
;;                   (goto-char (match-end 0)))))
;;     (if srch
;;         (goto-char (match-beginning 0))
;;       (goto-char pos)
;;       (setq org-link-search-failed t)
;;       (error "No further link found"))))

;; (defun org-previous-visible-link ()
;;   "Move backward to the previous link.
;; If the link is in hidden text, expose it."
;;   (interactive)
;;   (when (and org-link-search-failed (eq this-command last-command))
;;     (goto-char (point-max))
;;     (message "Link search wrapped back to end of buffer"))
;;   (setq org-link-search-failed nil)
;;   (let* ((pos (point))
;;          (ct (org-context))
;;          (a (assoc :link ct))
;;          srch)
;;     (if a (goto-char (nth 1 a)))
;;     (while (and (setq srch (re-search-backward org-any-link-re nil t))
;;                 (goto-char (match-beginning 0))
;;                 (not (eq (org-invisible-p) 'org-link))))
;;      (if srch
;;         (goto-char (match-beginning 0))
;;       (goto-char pos)
;;       (setq org-link-search-failed t)
;;       (error "No further link found"))))

;; ;; (define-key org-mode-map "\M-n" 'org-next-visible-link)
;; ;; (define-key org-mode-map "\M-p" 'org-previous-visible-link)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; TODOの設定 --- tamura70ブログ
;; ;;; http://d.hatena.ne.jp/tamura70/20100207/org
;; ;;; http://d.hatena.ne.jp/tamura70/20100215/org
;; ;;; TODOの状態の設定
;; ;;; ! をつけることで、その状態へ変更した日時を記録することが可能
;; ;;; @ をつけることで、その状態へ変更した時にメモを残すことが可能
;; (setq org-todo-keywords
;;       '((sequence "APPT(a)" "TODO(t)" "STARTED(s!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCEL(c@/!)")))
;; (setq org-log-done 'time)   ;;; DONEの時刻を記録
;; ;; (setq org-log-done 'note)  ;;; DONEの時刻とメモを記録


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; TAGの設定 --- tamura70のブログ
;; ;;; http://d.hatena.ne.jp/tamura70/20100215/org
;; ;;; TAGリストの一括設定
;; ;;; orgファイル毎で設定する場合は，ファイル中に以下のように記述する．
;; ;;; #+TAGS: @OFFICE(o) @HOME(h)
;; ;;; #+TAGS: SHOPPING(s) MAIL(m) PROJECT(p)
;; (setq org-tag-alist
;;   '(("@OFFICE" . ?o) ("@HOME" . ?h)
;;     ("MAIL" . ?m) ("WRITE" . ?w)
;;     ("ASK" . ?a)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; AGENDAの設定 --- tamura70ブログ
;; ;;; http://d.hatena.ne.jp/tamura70/20100208/org
;; ;; アジェンダ表示の対象ファイル
;; (setq org-agenda-files (list org-directory))
;; ;; アジェンダ表示で下線を用いる
;; (add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
;; ;; (setq hl-line-face 'underline)
;; ;; 標準の祝日を利用しない
;; (setq calendar-holidays nil)

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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; bpe.el --- Blog from Org mode to Blogger
;; ;;; see https://github.ctayamada/bpe
;; (use-package bpe
;;   :disabled t
;;   :config
;;   (require 'htmlize nil 'noerror) ; to fontify source code block on your blog.
;;   (setq bpe:account "shotakaha@gmail.com")
;;   (setq bpe:blog-name "KumaNote")
;;   (define-key org-mode-map (kbd "C-c C-p") 'bpe:post-article)
;;   (define-key org-mode-map (kbd "C-c C-i") 'bpe:insert-template)
;;   ;; For Japanese, default is $LANG environment variable.
;;   (setq bpe:lang "ja_JP.UTF-8")
;; )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; try weblogger.el
;; ;;; -> 動かないことが判明
;; (use-package weblogger
;;   :disabled t
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; org-gcal
;; ;;; see https://github.com/myuhe/org-gcal.el
;; (use-package org-gcal
;;   :disabled t
;;   :config
;;   (setq org-gcal-client-id "380736056994-68gg1ekgq85tdr0jdip8ionq7cnv5ggt.apps.googleusercontent.com"
;;         org-gcal-client-secret "vkwqCFKOifR31x8WBjb1IfIg"
;;         org-gcal-file-alist '(  ;; ("calendar-cal-id" . "filename")
;;                               ("shotakaha@gmail.com" . "~/Documents/org/home.org")
;;                               ("baiqrr0birr5g3n14pg6hkdf9s@group.calendar.google.com" . "~/Documents/org/work.org")
;;                               ))
;; )

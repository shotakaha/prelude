;;; my-editor.el --- basic Emacs settings

;;; Commentary:
;; とりあえずなんでも放り込め
;;
;; 1. view-modeの設定（less-like, vi-like なキーバインドの追加）
;; 2. origamiの設定
;; 3. ido-vertical-modeの設定 --> my-ido.el (2015-10-10)
;; 5. web-modeの設定
;; 6. jawordの設定
;; 7. symonの設定 --> enabled (2015-03-18)
;; 8. magitの設定 --> my-magit.el (2015-07-04)
;; 9. mag-menuの設定
;; 10. github-browse-fileの設定
;; 11. helm-cmd-tの設定（予定）
;; 12. git-gutterの設定（更なる詳細設定はGitHubを確認）--> my-magit.el (2015-10-10)
;; 13. visible-markの設定
;; 14. github-browse-fileの設定
;; 15. persp-modeの設定
;; 16. auto-completeの設定
;; 17. ac-mozcの設定
;; 18. ignoramusの設定
;; 19. smart-mode-line / rich-minorityの設定
;; 20. multiple-cursor-modeの設定
;; 22. showkeyの導入
;; 23. visual-regexp-steroidsの導入
;; 24. cbm の導入

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; undo
;; redo+
;; undohist
;; undo-tree
;; point-undo
;; goto-chg


;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Default packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bs
;;; C-x C-b binded to helm-buffer-list in Prelude
;;; (bind-key "C-x C-b" 'bs-show)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ffap : C-x C-f に機能を追加する
;;; C-x C-f : helm-find-files に設定済み
(ffap-bindings)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mouse-wheel
(setq mouse-wheel-scroll-amount '(1 ((shift) . 2) ((control))))
(setq mouse-wheel-progressive-speed nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; uniquify.el
;;; Easy to recognize same-named file in different directory
;;; Already loaded in core/prelude-editor.el
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; midnight.el
;;; しばらく使ってないバッファを削除する
(use-package midnight
  :ensure t
  :config
  (midnight-delay-set 'midnight-delay "4:30am")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; which-key.el
;;; guide-key.elの進化版
;;; コマンド入力の補助候補をポップアップで表示
(use-package which-key
  :ensure t
  :config
  (which-key-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; view-mode
;;; るびきち : Emacsテクニックバイグル : p.217
;;; るびきち : Emacsテクニックバイグル : p.218
;; Open with 'C-x C-r'
;; Toggle with 'C-x C-q'
;; Enable view-mode

(use-package view
  :init
  (setq view-read-only t)
  :config
  (bind-keys :map view-mode-map
             ;; less-like
             ("N" . View-search-last-regexp-backward)
             ("?" . View-search-regexp-backward)
             ("G" . View-goto-line)
             ("b" . View-scroll-page-backward)
             ("f" . View-scroll-page-forward)
             ("d" . View-scroll-half-page-forward)
             ("u" . View-scroll-half-page-backward)
             ;;vi/w3m-like
             ("h" . backward-char)
             ("j" . next-line)
             ("k" . previous-line)
             ("l" . forward-char)
             ("J" . View-scroll-line-forward)
             ("K" . View-scroll-line-backward)
             )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; viewer.el --- improve view-mode
;;; http://emacswiki.org/emacs/viewer.el
(use-package viewer
  :ensure t
  :config
  (viewer-stay-in-setup)
  (viewer-change-modeline-color-setup)
  (viewer-aggressive-setup 'force)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; origami.el
;;; http://rubikitch.com/2015/01/03/origami/
;;; http://rubikitch.com/2015/01/05/origami-2/
(use-package origami
  :ensure t
  :config
  ;; (makunbound 'origami-view-mode-map)
  (define-minor-mode origami-view-mode
    "TABにorigamiの折畳みを割り当てる"
    nil "折紙"
    '(("\C-i" . origami-cycle))
    (or origami-mode (origami-mode 1)))

  (defun origami-cycle (recursive)
    "origamiの機能をorg風にまとめる"
    (interactive "P")
    (call-interactively
     (if recursive 'origami-toggle-all-nodes 'origami-toggle-node)))

  (defun view-mode-hook--origami ()
    (when (memq major-mode (mapcar 'car origami-parser-alist))
      (origami-view-mode (if view-mode 1 -1))))

  (add-hook 'view-mode-hook 'view-mode-hook--origami)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; web-mode.el
;;; http://web-mode.org
;;; インデントの設定とか、いろいろhookに足してみる
(use-package web-mode
  :config
  (setq web-mode-engines-alist
        '(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\.")))

  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-style-padding 1)
    (setq web-mode-script-padding 1)
    (setq web-mode-block-padding 0)
    (setq web-mode-comment-style 2)
    )
  (add-hook 'web-mode-hook  'my-web-mode-hook)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ac-html-csswatcher / ac-html-bootstrap
;;; https://github.com/osv/ac-html-csswatcher
;;; https://github.com/osv/ac-html-bootstrap
(use-package ac-html-csswatcher
  :disabled t
  :ensure t
  :config
  (company-web-csswatcher-setup)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; jaword
;;; http://rubikitch.com/2015/01/27/jaword/
;;; http://qiita.com/zk_phi/items/b9c90014c434b75e8139
;;; https://github.com/zk-phi/jaword
(use-package jaword
  :disabled t
  :ensure t
  :config
  (global-jaword-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; recentf-ext : 最近使ったファイルを開く
;;; recentf は Preludeで定義済み
(use-package recentf
  :config
  ;; 最近使ったファイルに加えないファイルを正規表現で指定する
  (setq recentf-exclude '("/TAGS$" "/var/tmp/"))
  )
(use-package recentf-ext :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; カーソル移動
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; point-undo : カーソルいちを戻す
(use-package point-undo
  :ensure t
  :bind (([f7] . point-undo)
         ([M-f7] . point-redo))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; goto-chg : 編集箇所の履歴をたどる
(use-package goto-chg
  :ensure t
  :bind (([f8] . goto-last-change)
         ([M-f8] . goto-last-change-reverse))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bm : 現在位置をハイライト付きで永続的に記憶させる
(use-package bm
  :ensure t
  :bind (("S-M-SPC" . bm-toggle)
         ("M-[" . bm-previous)
         ("M-]" . bm-next)
         )
  :init
  ;;; マークを保存する
  (setq-default bm-buffer-persistence t)
  ;;; 保存先ファイル名
  (setq bm-repository-file (expand-file-name "bm-repository" prelude-savefile-dir))
  :config
  ;;; 起動時に設定を読み込み
  (setq bm-restore-repository-on-load t)
  (add-hook 'find-file-hook 'bm-buffer-restore)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  ;;; 終了時などに保存
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'vc-before-checkin-hook 'bm-buffer-save)
  ;;; Emacs終了時に保存
  (add-hook 'kill-emacs-hook '(lambda nil (bm-buffer-save-all) (bm-repository-save)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; imenu : 見出し・関数定義にジャンプする

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 編集
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; visual-regexp : 正規表現置換を対話的に行う
;;; http://rubikitch.com/2014/08/26/visual-regexp/
;;; visual-regexp-steroids.el
;;; http://rubikitch.com/2015/04/20/visual-regexp-steroids/
(use-package visual-regexp-steroids
  :ensure t
  :init
  (use-package visual-regexp :ensure t)
  :bind (
         ("M-%" . vr/query-replace)
         ("C-c m" . vr/mc-mark)
         ("C-M-r" . vr/isearch-backward)
         ("C-M-s" . vr/isearch-forward))
  :config
  (setq vr/engine 'python)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rectangle-mark-mode : 矩形編集
;;; C-x SPC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; browse-kill-ring : kill-ringをフル活用
;;; core/prelude-editor で定義済み
;;; prelude-key-chord.el でバインド済み
(use-package browse-kill-ring
  :disabled t
  :ensure t
  :bind(("M-y" . browse-kill-ring)
        )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; symon.el
;;; http://rubikitch.com/2015/02/01/symon/
;;; http://qiita.com/zk_phi/items/9576874a35a2affbefa8
;;; https://github.com/zk-phi/symon
;;; 2015-03-18 : darwin support されてたのでONにした
(use-package symon
  :ensure t
  :config
  ;; (symon-mode)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zop-to-char
;;; core/prelude-global-keybindings.el で設定済みだがもう一度
(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char))
  )

(use-package zzz-to-char
  :disabled t
  :ensure t
  :bind (("M-z" . zzp-up-to-char)
         ("M-Z" . zzp-to-char))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mag-menu.el
;;; http://rubikitch.com/2014/12/20/mag-menu/
;;; https://github.com/chumpage/mag-menu
(use-package mag-menu
  :disabled t
  :ensure t
  :config
  ;; 単にメッセージを表示するだけのシンプルなメニュー
  (defun mag-menu-test ()
    (interactive)
    (mag-menu
     '(test
       (actions
        ("c" "Commit" mag-menu-test-commit)
        ("l" "Log" mag-menu-test-log)))))
  (defun mag-menu-test-commit (options)
    (message "Commit"))
  (defun magi-menu-test-log (options)
    (message "Log"))

  ;; Google検索の例
  (defun google-menu (query)
    "Google検索のメニュー"
    (interactive "sGoogle: ")
    (mag-menu
     '(google                             ;任意の名前
       (actions
        ("g" "検索！" google-menu-search) ;一番上はRETで実行可能
        ("u" "URLを見る" google-menu-show-url)
        ("o" "オプションを見る" google-menu-show-options))
       (switches
        ("j" "日本語のみ" "--only-japanese"))
       (arguments                         ;オプション名は=で終わる
        ("s" "期間" "--period=" mag-menu-read-generic)
        ("Q" "検索語" "--query=" mag-menu-read-generic)))
     `(("--only-japanese")
       ("--period" . "y5")                ;ここでは=はつけない
       ("--query" . ,query)))
    )
  (defun google-menu-search (options)
    "Google検索する"
    (interactive)
    (browse-url (google-menu-url options))
    )
  (defun google-menu-show-url (options)
    "GoogleのURLを表示する"
    (interactive)
    (message "%s" (google-menu-url options))
    )
  (defun google-menu-show-options (options)
    "オプションを表示する"
    (interactive)
    (message "%S" options)
    )
  (defun google-menu-url (options)
    "OPTIONSに応じてGooleのURLを求める"
    (format "http://www.google.co.jp/search?q=%s&hl=ja&as_qdr=%s%s"
            (url-hexify-string (assoc-default "--query" options))
            (assoc-default "--period" options)
            (if (assoc-default "--only-japanese" options)
                "&lr=lang_ja"
              ""))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; github-browse-file
;;; http://rubikitch.com/2014/11/01/github-browse-file/
;;; https://github.com/osener/github-browse-file
(use-package github-browse-file
  :ensure t
  :config
  (setq github-browse-file-show-line-at-point t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-cmd-t
;;; http://rubikitch.com/2014/11/15/helm-cmd-t/
(prelude-require-package 'helm-cmd-t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; visible-mark.el
;;; http://rubikitch.com/2015/02/05/visible-mark/
(use-package visible-mark
  :ensure t
  :config
  (setq set-mark-command-repeat-pop t)
  (setq visible-mark-max 10)
  (global-visible-mark-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; github-browse-file
;;; https://github.com/osener/github-browse-file
;;; http://ozansener.com/blog/view-the-file-youre-editing-in-emacs-on-github/
;;; http://rubikitch.com/2014/11/01/github-browse-file/
(use-package github-browse-file
  :ensure t
  :config
  (setq github-browse-file-show-line-at-point t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; persp-mode
;;; http://rubikitch.com/2015/02/13/persp-mode/
;;; https://github.com/Bad-ptr/persp-mode.el
(use-package persp-mode
  :disabled t
  :ensure t
  :config
  ;; (with-eval-after-load "persp-mode-autoloads"
  ;;   (setq wg-morph-on nil) ;; switch off animation of restoring window configuration
  ;;   (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
  ;; Set prefix key (default)
  (setq persp-keymap-prefix (kbd "C-c p"))
  ;; バッファを切り替えたら見えるようにする
  (setq persp-add-on-switch-or-display t)
  (persp-mode 1)
  (defun persp-register-buffers-on-create ()
    (interactive)
    (dolist (bufname (condition-case _
                         (helm-comp-read
                          "Buffers: "
                          (mapcar 'buffer-name (buffer-list))
                          :must-match t
                          :marked-candidates t)
                       (quit nil)))
      (persp-add-buffer (get-buffer bufname))))
  (add-hook 'persp-activated-hook 'persp-register-buffers-on-create)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-complete
;;; http://rubikitch.com/2014/11/05/auto-complete/
;; (use-package auto-complete-config
;;   :disabled t
;;   :config
;;   (ac-config-default)
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ac-mozc
;;; https://github.com/igjit/ac-mozc
;; (use-package ac-mozc
;;   :disabled t
;;   :config
;;   (bind-keys :map ac-mode-map
;;              ("C-c C-SPC" . ac-complete-mozc)
;;              )
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package comment-dwim-2
  :ensure t
  :bind (("M-;" . comment-dwim-2))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ov --- overlay
;;; https://github.com/ShingoFukuyama/ov.el
;;; http://rubikitch.com/2015/02/16/ov/
;;; 句点で改行をoverlayすることで、HTML編集を綺麗にする。
;;; web-mode に hookしてもいいのかも。
(defvar-local ja-period-newline-overlays nil)
(use-package ov
  :config
  ;; バッファローカル変数を宣言
  ;; defvar + make-variable-buffer-local
  (define-minor-mode ja-period-newline-mode
    "。の後に改行を入れてよみやすくする"
    nil "。\\n" nil
    (if ja-period-newline-mode
        ;; 有効にしたときは
        (setq ja-period-newline-overlays
              ;; [。]をすべて検索し、改行を付加するオーバーレイを作成する
              (ov-set "。" 'after-string "\n"))
      ;; 無効にしたときは全オーバーレイを削除する
      (mapc 'delete-overlay ja-period-newline-overlays))
    )
  ;; (provide 'mylisp-ja-period-newline)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ignoramous
;;; https://github.com/rolandwalker/ignoramus
;;; http://rubikitch.com/2015/02/19/ignoramus/
(use-package ignoramus
  :ensure t
  :config
;;  (require 'dired-x)
  (ignoramus-setup)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; multiple-cursor-mode
;;; https://github.com/magnars/multiple-cursors.el
;;; http://rubikitch.com/2014/11/10/multiple-cursors/
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-x r t" . mc/edit-lines-or-string-rectangle)
         ("C-M-SPC" . mc/mark-all-dwim-or-mark-sexp)
         )
  :config
  (setq mc/list-file (expand-file-name "multiple-cursors" prelude-savefile-dir))
  (defun mc/edit-lines-or-string-rectangle (s e)
    "C-x r tで同じ桁の場合にmc/edit-lines (C-u M-x mc/mark-all-dwim)"
    (interactive "r")
    (if (eq (save-excursion (goto-char s) (current-column))
            (save-excursion (goto-char e) (current-column)))
        (call-interactively 'mc/edit-lines)
      (call-interactively 'string-rectangle)))

  (defun mc/mark-all-dwim-or-mark-sexp (arg)
    "C-u C-M-SPCでmc/mark-all-dwim, C-u C-u C-M-SPCでC-u M-x mc/mark-all-dwim"
    (interactive "p")
    (cl-case arg
      (16 (mc/mark-all-dwim t))
      (4 (mc/mark-all-dwim nil))
      (1 (mark-sexp 1))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; phi-search
;;; multiple-cursors + isearch
(use-package phi-search
  :disabled t
  :ensure t
  :bind (("C-s" . phi-search)
         ("C-r" . phi-search-backward)
         )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package phi-search-migemo
  :disabled t
  :ensure t
  :config
  (bind-key "M-m" 'phi-search-migemo-toggle phi-search-default-map)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http://rubikitch.com/2015/04/05/phi-search-dired/
(use-package phi-search-dired
  :disabled t
  :ensure t
  :bind (:map dired-mode-map
              ("C-s" . phi-search-dired)
              )
  :config
  (defun phi-search-dired-restrict-to-matches--show-all ()
    (with-selected-window (minibuffer-selected-window)
      (when (re-search-backward " \\.\\./?$" nil t)
        (forward-line 1)
        (recenter nil))))
  (advice-add 'phi-search-dired-restrict-to-matches :after
              'phi-search-dired-restrict-to-matches--show-all)

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; showkey.el
;;; http://rubikitch.com/2015/04/19/showkey/
(use-package showkey
  :ensure t
  :config
  (push '(font . "Ricty-13") showkey-log-frame-alist)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; crosshaires.el
(use-package crosshairs
  :ensure t
  :config
  (crosshairs-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 参考にした設定 : http://rakkyoo.net/?p=1444
(use-package twittering-mode
  :ensure t
  :config
  ;; 簡単ログインの設定
  (setq twittering-allow-insecure-server-cert t)
  (setq twittering-use-master-password t)
  ;; (setq twittering-private-info-file "~/.emacs.d/twittering-mode.gpg")
  ;; (setq twittering-status-format "%i @%s %S %p: n %T  [%@]%r %R %f%Ln ------------------------------" )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stripe-buffer.el
;;; http://rubikitch.com/2014/11/30/stripe-buffer/
;;; Set stripes to buffers such as Dired-mode, Org-mode, etc.
;;; by adding hooks
(use-package stripe-buffer
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'stripe-listify-buffer)
  (add-hook 'org-mode-hook 'turn-on-stripe-table-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cbm.el
;;; https://github.com/akermu/cbm.el
(use-package cbm
  :disabled t
  :ensure t
  :bind (("M-%" . vr/query-replace)
         )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; redo+.el
(use-package redo+
  :ensure t
  :bind (("C-M-/" . redo))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; undohist.el
;;; undohist-directory : ~/.emacs.d/undohist/
(use-package undohist
  :ensure t
  :config
  (setq undohist-ignored-files '("/tmp/" "EDITMSG" "/elpa/" "gpg$"))
  (undohist-initialize)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; undo-tree.el
(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-mode-lighter "")
  (global-undo-tree-mode 1)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; linum-relative.el
;;; linum-on だけだと動作しないので、
;;; linum-relative-global-modeを付けてある
(use-package linum-relative
  :ensure t
  :config
  (linum-on)
  (linum-relative-global-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; highlight-symbol.el
(use-package highlight-symbol
  :ensure t
  :bind (("M-s M-r" . highlight-symbol-query-replace) )
  :config
  ;;; 1秒後自動ハイライトされるようになる
  (setq highlight-symbol-idle-delay 1.0)
  ;;; 自動ハイライトをしたいならば
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  ;;; ソースコードにおいてM-p/M-nでシンボル間を移動
  (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sync-recentf.el
(use-package sync-recentf
  :ensure t
  :config
  (setq recentf-auto-cleanup 60)
  (recentf-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :bind (:map yas-minor-mode-map
              ("C-x i i" . yas-insert-snippet)
              ("C-x i n" . yas-new-snippet)
              ("C-x i v" . yas-visit-snippet-file)
              ("C-x i l" . yas-describe-tables)
              ("C-x i g" . yas-reload-all)
              )
  :config
  (yas-global-mode 1)
  (setq yas-prompt-functions '(yas-ido-prompt))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package hippie-exp
  :config
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; quick-preview
(use-package quick-preview
  :ensure t
  :bind (("C-c q" . quick-preview-at-point))
  :config
  (bind-key "SPC" 'quick-preview-at-point dired-mode-map)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; beacon
(use-package beacon
  :ensure t
  :disabled t
  :config
  (beacon-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vline
(use-package vline
  :ensure t
  :disabled t
  :config
  (vline-global-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shell-pop
(use-package shell-pop
  :ensure t
  :bind* ("C-c t" . shell-pop)
  :config
  (setq shell-pop-term-shell "/opt/local/bin/zsh")
  (setq shell-pop-full-span t)
  (setq shell-pop-window-size 20)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; smartrep
(bind-key* "C-," 'repeat)
(use-package smartrep
  :ensure t
  :config
  (smartrep-define-key global-map "C-x"
    '(("o" . other-window)
      ("0" . delete-window)
      ("1" . delete-other-windows)
      ("2" . split-window-below)
      ("3" . split-window-right)
      ("{" . shrink-window-horizontally)
      ("}" . enlarge-window-horizontally)
      ("+" . balance-windows)
      ("^" . enlarge-window)
      ("-" . shrink-window)
      ("p" . git-gutter:previous-hunk)
      ("n" . git-gutter:next-hunk)
      )
    )
  (smartrep-define-key org-mode-map "C-c"
    '(("C-n" . org-next-visible-heading)
      ("C-p" . org-previous-visible-heading)
      ("C-u" . outline-up-heading)
      ("C-f" . org-forward-heading-same-level)
      ("C-b" . org-backward-heading-same-level)
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; avy
(use-package avy
  :ensure t
  :bind (("C-c j" . avy-goto-word-or-subword-1)
         ("s-." . avy-goto-word-or-subword-1)
         )
  ;; :chords (("jj" . avy-goto-word-1)
  ;;          ("jl" . avy-goto-line)
  ;;          ("jk" . avy-goto-char)
  ;;          )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; swiper
(use-package swiper
  :disabled t
  :ensure t
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-c C-r" . ivy-resume)
         ;; ("[f6]" . ivy-resume)
         )
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; swoop
(use-package swoop
  :disabled t
  :ensure t
  :bind (("C-o" . swoop)
         ("C-M-o" . swoop-multi)
         ("M-o" . swoop-pcre-regexp)
         ("C-S-o" . swoop-back-to-last-position)
         ("s-6" . swoop-migemo)
         )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; smart-newline
(use-package smart-newline
  :disabled t
  :ensure t
  :bind ("C-m" . smart-newline)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; company
;;; modules/prelude-company.el で設定済み
;;; http://qiita.com/syohex/items/8d21d7422f14e9b53b17
(use-package company
  :bind ("C-M-i" . company-complete)
  :config
  (bind-keys :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("C-s" . company-filter-candidates)
             ("C-i" . company-complete-selection)
             )
  (bind-keys :map company-search-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             )
  (bind-keys :map emacs-lisp-mode-map
             ("C-M-i" . company-complete)
             )

  )
(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode +1)
  )

(use-package path-headerline-mode
  :ensure t
  :config
  (path-headerline-mode +1)
)

(use-package powerline
  :config
  (powerline-center-theme)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; smartparens
;;; https://github.com/Fuco1/smartparens
;;; https://ebzzry.github.io/emacs-pairs.html
;;; core/prelude-editor.el で設定済み
(use-package smartparens-config
  :ensure smartparens
  :config
  (show-smartparens-global-mode t)

  (bind-keys :map smartparens-mode-map
             ("C-M-a" . sp-beginning-of-sexp)
             ("C-M-e" . sp-end-of-sexp)

             ("C-<down>" . sp-down-sexp)
             ("C-<up>"   . sp-up-sexp)
             ("M-<down>" . sp-backward-down-sexp)
             ("M-<up>"   . sp-backward-up-sexp)

             ("C-M-f" . sp-forward-sexp)
             ("C-M-b" . sp-backward-sexp)

             ("C-M-n" . sp-next-sexp)
             ("C-M-p" . sp-previous-sexp)

             ("C-S-f" . sp-forward-symbol)
             ("C-S-b" . sp-backward-symbol)

             ("C-<right>" . sp-forward-slurp-sexp)
             ("M-<right>" . sp-forward-barf-sexp)
             ("C-<left>"  . sp-backward-slurp-sexp)
             ("M-<left>"  . sp-backward-barf-sexp)

             ("C-M-t" . sp-transpose-sexp)
             ("C-M-k" . sp-kill-sexp)
             ("C-k"   . sp-kill-hybrid-sexp)
             ("M-k"   . sp-backward-kill-sexp)
             ("C-M-w" . sp-copy-sexp)

             ("C-M-d" . delete-sexp)

             ("M-<backspace>" . backward-kill-word)
             ("C-<backspace>" . sp-backward-kill-word)
             ([remap sp-backward-kill-word] . backward-kill-word)

             ("M-[" . sp-backward-unwrap-sexp)
             ("M-]" . sp-unwrap-sexp)

             ("C-x C-t" . sp-transpose-hybrid-sexp)

             ("C-c ("  . wrap-with-parens)
             ("C-c ["  . wrap-with-brackets)
             ("C-c {"  . wrap-with-braces)
             ("C-c '"  . wrap-with-single-quotes)
             ("C-c \"" . wrap-with-double-quotes)
             ("C-c _"  . wrap-with-underscores)
             ("C-c `"  . wrap-with-back-quotes)
             )

  ;; (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  ;; (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)
  )

(use-package selected
  :ensure t
  :commands selected-minor-mode
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("u" . upcase-region)
              ("d" . downcase-region)
              ("w" . count-words-region)
              ("m" . apply-macro-to-region-lines))
  )

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1)
  )

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-items '((projects . 10)
                          (recents . 5)
                          (bookmarks . 5)))
  (dashboard-setup-startup-hook)
  )

(use-package zoom-window
  :ensure t
  :bind (("C-x 1" . zoom-window-zoom))
  )

(use-package reveal-in-osx-finder
  :ensure t
  )

(provide 'my-editor)
;;; my-editor.el ends here

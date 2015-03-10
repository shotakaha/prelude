;;; my-editor.el --- basic Emacs settings

;;; Commentary:
;; とりあえずなんでも放り込め
;;
;; 1. view-modeの設定（less-like, vi-like なキーバインドの追加）
;; 2. origamiの設定
;; 3. ido-vertical-modeの設定
;; 4. psessionの設定
;; 5. web-modeの設定
;; 6. jawordの設定
;; 7. symonの設定（予定）
;; 8. magitの設定
;; 9. mag-menuの設定
;; 10. github-browse-fileの設定
;; 11. helm-cmd-tの設定（予定）
;; 12. git-gutterの設定（更なる詳細設定はGitHubを確認）
;; 13. visible-markの設定
;; 14. github-browse-fileの設定
;; 15. persp-modeの設定
;; 16. auto-completeの設定
;; 17. ignoramousの設定

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; view-mode
;;; るびきち : Emacsテクニックバイグル : p.217
;;; るびきち : Emacsテクニックバイグル : p.218
;; Open with 'C-x C-r'
;; Toggle with 'C-x C-q'
;; Enable view-mode
(setq view-read-only t)
(use-package view
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
;;; るびきち : Emacsテクニックバイグル : p.220
;;; p.220 - view-mode の mode-line に色を付ける
;;; p.221 - メジャーモードに合わせて view-mode のキーバインドを設定する
;;; p.222 - 特定のファイルを view-mode で開く

(use-package viewer
  :config
  (viewer-stay-in-setup)
  ;; set color to mode-line
  ;; 書き込み不可の場合 : tomato
  (setq viewer-modeline-color-unwritable "tomato")
  ;; view-mode の場合 : orange
  (setq viewer-modeline-color-view "orange")
  (viewer-change-modeline-color-setup)

  ;; set view-mode kbd according to major-mode
  ;; カーソル位置の関数定義にジャンプするコマンドをRETに統一
  (define-overriding-view-mode-map c-mode
    ("RET" . gtags-find-tag-from-here))
  (define-overriding-view-mode-map emacs-lisp-mode
    ("RET" . find-function-at-point))

  ;; ログファイル（.log）は view-mode 開く
  (setq view-mode-by-default-regexp "\\.log$")
  ;; すべてファイルを view-mode で開く
  ;; ただし、文字数の少ないファイルは除く（デフォルト100文字以下）
  ;; (viewer-aggressive-minimum-size 100)
  ;; (viewer-aggressive-setup 'force)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; origami.el
;;; http://rubikitch.com/2015/01/03/origami/
;;; http://rubikitch.com/2015/01/05/origami-2/
(prelude-require-package 'origami)
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
;;; ido - ファイル・バッファを選択して開く
;;; coreで定義済み
;; (ido-mode 1)
;; (ido-everywhere 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ido-vertical-mode.el
;;; http://rubikitch.com/2015/01/06/ido-vertical-mode/
(prelude-require-package 'ido-vertical-mode)
(use-package ido-vertical-mode
  :ensure t
  :config
  ;; このときidoが使うwindowの高さは大きくした方がいい
  (setq ido-max-window-height 0.75)
  ;; あいまいマッチは入れておこう
  (setq ido-enable-flex-matching t)
  (ido-mode 1)
  (ido-vertical-mode 1)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; psession.el
;;; http://rubikitch.com/2014/08/21/psession/
;;; https://github.com/thierryvolpiatto/psession
;;; package.elで入れたので、autoload はしなくていいはず
;;; セッション情報は、~/.emacs.d/elisp-objects/ に .elcファイルで保存。
;;; defcustom されている psession-elisp-objects-default-directory を
;;; 再定義すれば、任意の場所に変更できるはず。
(prelude-require-package 'psession)
(use-package psession
  :ensure t
  :config
  (psession-mode 1)
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
;;; jaword
;;; http://rubikitch.com/2015/01/27/jaword/
;;; http://qiita.com/zk_phi/items/b9c90014c434b75e8139
;;; https://github.com/zk-phi/jaword
(prelude-require-package 'jaword)
(use-package jaword
  :ensure t
  :config
  (global-jaword-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ファイル・バッファの切替
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bs : 手軽にバッファ選択
;;; C-x C-b : helm-buffer-list に設定済み
;; (global-set-key (kbd "C-x C-b") 'bs-show)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ffap : C-x C-f に機能を追加する
;;; C-x C-f : helm-find-files に設定済み
(ffap-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; recentf-ext : 最近使ったファイルを開く
;;; recentf は Preludeで定義済み
;; 最近のファイル500個を保存する
(setq recentf-max-saved-items 500)
;; 最近使ったファイルに加えないファイルを
;; 正規表現で指定する
(setq recentf-exclude
      '("/TAGS$" "/var/tmp/")
      )
(use-package recentf-ext
  :ensure t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; カーソル移動
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; point-undo : カーソルいちを戻す
(use-package point-undo
  :ensure t
  :config
  (global-set-key [f7] 'point-undo)
  (global-set-key [M-f7] 'point-redo)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; goto-chg : 編集箇所の履歴をたどる
(use-package goto-chg
  :ensure t
  :config
  (global-set-key [f8] 'goto-last-change)
  (global-set-key [M-f8] 'goto-last-change-reverse)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bm : 現在位置をハイライト付きで永続的に記憶させる
(setq-default bm-buffer-persistence nil)
(setq bm-restore-repository-on-load t)
(use-package bm
  :ensure t
  :bind (("M-SPC" . bm-toggle)
         ;; ("M-" . bm-previous)
         ;; ("M-" . bm-next)
         )
  :config
  (add-hook 'find-file-hook 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook 'bm-buffer-save)
  (add-hook 'kill-emacs-hook '(lambda nil (bm-buffer-save-all) (bm-repository-save)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; imenu : 見出し・関数定義にジャンプする

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 編集
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; visual-regexp : 正規表現置換を対話的に行う
(use-package visual-regexp
  :ensure t
  :config
  (global-set-key (kbd "M-%") 'vr/query-replace)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rectangle-mark-mode : 矩形編集
;;; C-x SPC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; browse-kill-ring : kill-ringをフル活用
;;; core/prelude-editor で定義済み
;;; prelude-key-chord.el でバインド済み
(use-package browse-kill-ring
  :ensure t
  :bind(("M-y" . browse-kill-ring)
        )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; symon.el
;;; http://rubikitch.com/2015/02/01/symon/
;;; http://qiita.com/zk_phi/items/9576874a35a2affbefa8
;;; https://github.com/zk-phi/symon
(prelude-require-package 'symon)
;;; あとでMacの設定をする。もしくはアップデートされるまで待つ

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit.el
;;; http://rubikitch.com/2015/01/30/magit-time-format/
(use-package magit
  :config
  (defun magit-format-duration--format-date (duration spec width)
    (format-time-string "%y-%m-%dT%H:%M:%S"
                        (seconds-to-time (- (float-time) duration))))
  (advice-add 'magit-format-duration :override
              'magit-format-duration--format-date)
  (defun magit-log-margin-set-timeunit-width--fixed ()
    (setq magit-log-margin-timeunit-width 12))
  (advice-add 'magit-log-margin-set-timeunit-width :override
              'magit-log-margin-set-timeunit-width--fixed)
  (setq magit-log-margin-spec '(33 nil magit-duration-spec))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mag-menu.el
;;; http://rubikitch.com/2014/12/20/mag-menu/
;;; https://github.com/chumpage/mag-menu
(prelude-require-package 'mag-menu)
(use-package mag-menu
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
(prelude-require-package 'github-browse-file)
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
;;; git-gutter
;;; https://github.com/syohex/emacs-git-gutter
(prelude-require-package 'git-gutter)
(use-package git-gutter
  :ensure t
  :bind ( ("C-x C-g" . git-gutter:toggle)
          ("C-x v =" . git-gutter:popup-hunk)
          ;; Jump to next/previous hunk
          ("C-x p" . git-gutter:previous-hunk)
          ("C-x n" . git-gutter:next-hunk)
          ;; Stage current hunk
          ("C-x v s" . git-gutter:stage-hunk)
          ;; Revert current hunk
          ("C-x v r" . git-gutter:revert-hunk)
          )
  :config
  ;; If you enable global minor mode
  (global-git-gutter-mode t)
  ;; If you would like to use git-gutter.el and linum-mode
  (git-gutter:linum-setup)
  ;; If you enable git-gutter-mode for some modes
  (add-hook 'ruby-mode-hook 'git-gutter-mode)

  ;; You can change the signs and those faces.
  (custom-set-variables
   '(git-gutter:modified-sign "  ") ;; two space
   '(git-gutter:added-sign "++")    ;; multiple character is OK
   '(git-gutter:deleted-sign "--"))

  (set-face-background 'git-gutter:modified "purple") ;; background color
  (set-face-foreground 'git-gutter:added "green")
  (set-face-foreground 'git-gutter:deleted "red")

  ;; You can change minor-mode-name in mode-line to set git-gutter:lighter. Default is " GitGutter"
  ;; first character should be a space
  (custom-set-variables
   '(git-gutter:lighter " GG"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; visible-mark.el
;;; http://rubikitch.com/2015/02/05/visible-mark/
(use-package visible-mark
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
  :config
  (setq github-browse-file-show-line-at-point t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; persp-mode
;;; http://rubikitch.com/2015/02/13/persp-mode/
;;; https://github.com/Bad-ptr/persp-mode.el

(use-package persp-mode
  :config
  (with-eval-after-load "persp-mode-autoloads"
    (setq wg-morph-on nil) ;; switch off animation of restoring window configuration
    (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
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
(use-package auto-complete-config
  :config
  (ac-config-default)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prelude-require-package 'comment-dwim-2)
(use-package comment-dwim-2
  :config
  (bind-key "M-;" 'comment-dwim-2)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ov --- overlay
;;; https://github.com/ShingoFukuyama/ov.el
;;; http://rubikitch.com/2015/02/16/ov/
;;; 句点で改行をoverlayすることで、HTML編集を綺麗にする。
;;; web-mode に hookしてもいいのかも。
;; (defvar-local ja-period-newline-overlays nil)
;; (use-package ov
;;   :config
;;   ;; バッファローカル変数を宣言
;;   ;; defvar + make-variable-buffer-local
;;   (define-minor-mode ja-period-newline-mode
;;     "。の後に改行を入れてよみやすくする"
;;     nil " 。\\n" nil
;;     (if ja-period-newline-mode
;;         ;; 有効にしたときは
;;         (setq ja-period-newline-overlays
;;               ;; [。]をすべて検索し、改行を付加するオーバーレイを作成する
;;               (ov-set "。" 'after-string "\n"))
;;       ;; 無効にしたときは全オーバーレイを削除する
;;       (mapc 'delete-overlay ja-period-newline-overlays))
;;     )
;;   ;; (provide 'mylisp-ja-period-newline)
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ignoramous
;;; https://github.com/rolandwalker/ignoramus
;;; http://rubikitch.com/2015/02/19/ignoramus/
(use-package ignoramous
  :config
  (require 'dired-x)
  (require 'ignoramus)
  (ignoramus-setup)
)

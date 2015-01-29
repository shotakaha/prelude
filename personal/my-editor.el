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
  :config
  (add-hook 'find-file-hook 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook 'bm-buffer-save)
  (add-hook 'kill-emacs-hook '(lambda nil (bm-buffer-save-all) (bm-repository-save)))

  (global-set-key (kbd "M-SPC") 'bm-toggle)
  ;; (global-set-key (kbd "M-") 'bm-previous)
  ;; (global-set-key (kbd "M-") 'bm-next)
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
  :config
  (global-set-key (kbd "M-y") 'browse-kill-ring)
  )

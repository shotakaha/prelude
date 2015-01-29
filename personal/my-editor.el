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
(psession-mode 1)

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

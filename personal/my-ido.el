;;; my-ido.el --- my ido configuration

;;; Commentary:

;;; Ido関連のパッケージの設定

;;; Prelude では以下のファイルであるていど設定済み
;;; ../modules/prelude-ido.el (ido, ido-ubiquitous, flx-ido)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ido-vertical-mode.el
;;; https://github.com/creichert/ido-vertical-mode.el
;;; http://rubikitch.com/2015/01/06/ido-vertical-mode/
(use-package ido-vertical-mode
  :ensure t
  :config
  ;; このときidoが使うwindowの高さは大きくした方がいい
  (setq ido-max-window-height 0.75)
  ;; あいまいマッチは入れておこう
  (setq ido-enable-flex-matching t)
  (ido-mode 1)
  (ido-everywhere 1)
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (setq ido-vertical-show-count t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ido-select-window
;;; C-x o (other-window) をカイゼン
(use-package ido-select-window
  :ensure t
  :bind ("C-x o" . ido-select-window)
  )

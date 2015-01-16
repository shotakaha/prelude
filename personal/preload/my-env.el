;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; encoding
;; (set-language-environment 'Japanese)
;; (prefer-coding-system 'utf-8)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; 起動時の表示をオフにする
;; (setq inhibit-startup-screen t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; 対応する括弧を強調する
;; (show-paren-mode t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; Yes/No 入力を短くする
;; (defalias 'yes-or-no-p 'y-or-n-p)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; モードラインの表示設定
;; (display-time-mode 2)
;; (line-number-mode 2)
;; (column-number-mode 1)
;; (display-battery-mode 0)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; 矩形選択時にハイライトする（デフォルト？）
;; (transient-mark-mode 1)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; テキストをアンチエイリアスする
;; (setq ns-antialias-text t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; インデント時、TABの代わりにSPACEを使う
;; (setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)

;; ;;; TABサイズの設定（どれが効いているのかよく分からない）
;; (defvar c-default-style)
;; (defvar c-basic-offset)
;; (setq tab-width 4)
;; (setq c-default-style "k&r")
;; (setq c-basic-offset 4)
;; (setq show-trailing-whitespace t)

;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ;;; highlight current line
;;  (global-hl-line-mode 1)
;; (set-face-background 'hl-line "gray75")
;; ;;(set-face-background 'hl-line "selectedTextBackgroundColor")
;; ;;(set-face-background 'hl-line "controlHighLightColor")  ;; same as gray90 ?

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; DISABLE creating auto-save-file(#filename#)
;; ; (setq auto-save-default nil)
;; ; (setq auto-save-list-file-name nil)
;; ; (setq auto-save-list-file-prefix nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; DISABLE creating backup-files(filename~)
;; (setq make-backup-files nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; save time-stamp information automatically
;; (add-hook 'before-save-hook 'time-stamp)
;; (eval-after-load "time-stamp"
;;   '(progn
;;      (setq time-stamp-start "Last Update: ")
;;      (setq time-stamp-format "%04y/%02m/%02d @ %02H:%02M by %u @ %s\"")
;;      (setq time-stamp-end "$")
;;      (setq time-stamp-line-limit 10))) ; def=8

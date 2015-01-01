;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; smart-mode-line.el
(use-package smart-mode-line
  :disabled t
  :config
;;; この変数を定義しておかないとエラーになるバグあり
  (setq sml/active-background-color "gray60")
;;; 桁番号も表示させる
  (column-number-mode 1)
;;; 読み込み専用バッファは%で表示
  (setq sml/read-only-char "%%")
;;; 修正済みバッファは*で表示
  (setq sml/modified-char "*")
;;; helm-modeとauto-complete-modeのモードライン表示を隠す
  (setq sml/hidden-modes '(" Helm" " AC"))
;;; これがないと表示がはみでる
  (setq sml/extra-filler -10)
;;; sml/replacer-regexp-listはモードラインでのファイル名表示方法を制御
  (add-to-list 'sml/replacer-regexp-list '("^.+/junk/[0-9]+/" ":J:") t)
;;; これを入れないとsmart-mode-lineを読み込むたびに
;;; Loading a theme can run Lisp code.  Really load? (y or n)
;;; と聞いてくる。
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  ;;(sml/apply-theme 'respectful)
;;; その他のthemeを設定
  ;;(sml/apply-theme 'dark)
  ;;(sml/apply-theme 'light)
  (sml/apply-theme 'automatic)
)


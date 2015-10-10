;;; my-migemo --- my migemo configration

;;; Commentary:

;;; Migemoはローマ字入力（＝漢字変換なし）で日本語検索できる外部コマンド
;;; Rubyで書かれてたのをCに移植したのがC/Migemo（cmigemo）
;;; Emacsのパッケージ名は migemo.el

;;; 下準備として cmigemo をインストールする必要がある
;;; MacPortsにはないみたいなので、GitHubからソースコードをcloneしてビルドする
;;; 参考サイト : http://weblog.ymt2.net/blog/html/2013/08/23/install_migemo_to_emacs_24_3_1.html

;;; $ git clone https://github.com/koron/cmigemo.git
;;; $ cd cmigemo
;;; $ ./configure
;;; $ make osx
;;; $ make osx-dict
;;; $ sudo make osx-install

;;; cmigemo、migemo-dictの場所を確認
;;; $ which cmigemo
;;; => /usr/local/bin/cmigemo
;;; $ locate miemo-dict
;;; => /usr/local/share/migemo/utf-8/migemo-dict

;;; migemo.el の設定はるびきち日刊Emacsを参照
;;; 参考サイト : http://rubikitch.com/2014/08/20/migemo/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; migemo.el
(use-package migemo
  :ensure t
  :config
  (setq migemo-command "/usr/local/bin/cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  ;; Set your installed path
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init)
  )

(provide 'my-migemo)
;;; my-migemo.el ends here

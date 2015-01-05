;;; Mac用フォント設定
;;; http://tcnksm.sakura.ne.jp/blog/2012/04/02/emacs/
;;; http://d.hatena.ne.jp/excer/20110605/1307274305

;; 英語
(set-face-attribute 'default nil
                    :family "Ricty" ;; font
                    :height 210)    ;; font size

;; 日本語
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Ricty")) ;; font

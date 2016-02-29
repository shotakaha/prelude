;;; my-gnus.el --- my Gnus settings

;;; Commentary:
;;
;; Gnus を使って Gmail を読めるように設定
;; 1. はじめに
;;    - gnupg をインストールする（port install gnupg）
;;    - gnutls をインストールする（port install gnutls）
;;    - ~/.authinfo.gpg を用意しておく（下記参照）
;;
;; 2. authinfoの設定
;;    - gpgファイルにしておく（~/.authinfo.gpg）
;;    - このファイルを開くのに gnupg が必要
;;    - 以下の書式で記述する (Gmailを使う場合)
;;
;; machine imap.gmail.com login USERNAME@gmail.com password SECRET port 993
;; machine smtp.gmail.com login USERNAME@gmail.com password SECRET port 587
;;
;;    - imapのポート : 993
;;    - smtpのポート : 587
;;    - ２段階認証を設定している場合は SECRET の部分はアプリケーション固有パスワード
;;
;; 2. IMAP接続の設定
;;
;; 読むだけならここまででOK（M-x gnus して確かめる）
;;
;; 3. SMTPの設定
;;   - smtp-mail を使ってメールを送信する

;;; 使い方
;; 1. M-x gnus
;;      TLSで接続中・・・とでるので暫し待つ（数十秒？）
;;      ~/.newsrc.eld の読み込み開始
;;      Gmailを開く

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gnus Gmail
;;; http://www.emacswiki.org/emacs/GnusGmail

;;; Firstly, create ~/.authinfo.gpg and put lines below
;;; (~/.authinfo.gpg(=encrypted) is safer than ~/.authinfo(=plain text))
;;;
;;; Replace USERNAME with your own account
;;; Replace SECRET with your applicaiton-specified-password (for 2-step auth.)
;;; machine imap.gmail.com login USERNAME@gmail.com password SECRET port 993
;;; machine imap.gmail.com login USERNAME@gmail.com password SECRET port 587

(setq user-mail-address "shotakaha@gmail.com")

;;; Access Gmail via IMAP (as a primary method)
(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port "imaps")
               (nnimap-stream ssl)
               )
      )

;;; Gmail SMTP
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9]+\\( \\|$\\)\\|^[\"]\"[#`()]"
      )
;; smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;; smtpmail-auth-credentials '(("smtp.gmail.com" 587
;;   			   "user@gmail.com" nil))
;; smtpmail-default-smtp-server "smtp.gmail.com"
;; )

;;; Reply-to with the same address as it was sent to
(setq my-gmail "shotakaha@gmail.com")
(setq my-kek "shotakah@post.kek.jp")
(setq gnus-posting-styles
      '(((header "to" "shotakaha@gmail.com")
         (address "shotakaha@gmail.com"))
        ((header "cc" "shotakaha@gmail.com")
         (address "shotakaha@gmail.com"))
        ;;;
        ((header "to" "shotakah@post.kek.jp")
         (address "shotakah@post.kek.jp"))
        ((header "cc" "shotakah@post.kek.jp")
         (address "shotakah@post.kek.jp"))
        ))

(setq gnus-summary-line-format  "%U%R%z |%d| %I%(%[%4L: %-23,23f%]%) %s\n")

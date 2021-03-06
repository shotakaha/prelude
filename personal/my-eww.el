;;; my-eww.el -- Shota's configuration for EWW

;;; Commentary:
;;
;; EWWを使いやすくするための設定たち
;;
;; 1. EWWの履歴をhelm/anythingする設定
;; 2. eww-lnum の設定 → 次の ace-link に乗り換え
;; 3. ace-link の設定
;; 4. 画像を非表示にする方法
;; 5. 辞書（Weblio, Wikipedia）設定（を追加する予定）
;; 6. ブラウジング時の配色を見やすくなるように設定（うまく動かない）
;; 7. 複数のewwを開けるようにする

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-eww.el
;;; EWWの履歴をhelm/anythingする方法
;;; http://rubikitch.com/2014/11/26/helm-eww/
;;; H, s で履歴の表示

(use-package eww
  :config
  (bind-keys :map eww-mode-map
             ("h" . backward-char)
             ("j" . next-line)
             ("k" . previous-line)
             ("l" . forward-char)
             ("J" . View-scroll-line-forward)
             ("K" . View-scroll-line-backward)
             ("s-[" . eww-back-url)
             ("s-]" . eww-forward-url)
             ("s-{" . previous-buffer)
             ("s-}" . next-buffer)
             )
  )


(defvar eww-data)
(defun eww-current-url ()
  (if (boundp 'eww-current-url)
      eww-current-url                   ;emacs24.4
    (plist-get eww-data :url)))         ;emacs25
(defun eww-current-title ()
  (if (boundp 'eww-current-title)
      eww-current-title                   ;emacs24.4
    (plist-get eww-data :title)))

(require 'helm)
(require 'cl-lib)

(defun helm-eww-history-candidates ()
  (cl-loop with hash = (make-hash-table :test 'equal)
           for b in (buffer-list)
           when (eq (buffer-local-value 'major-mode b) 'eww-mode)
           append (with-current-buffer b
                    (clrhash hash)
                    (puthash (eww-current-url) t hash)
                    (cons
                     (cons (format "%s (%s) <%s>" (eww-current-title) (eww-current-url) b) b)
                     (cl-loop for pl in eww-history
                              unless (gethash (plist-get pl :url) hash)
                              collect
                              (prog1 (cons (format "%s (%s) <%s>" (plist-get pl :title) (plist-get pl :url) b)
                                           (cons b pl))
                                (puthash (plist-get pl :url) t hash)))))))

(defun helm-eww-history-browse (buf-hist)
  (if (bufferp buf-hist)
      (switch-to-buffer buf-hist)
    (switch-to-buffer (car buf-hist))
    (eww-save-history)
    (eww-restore-history (cdr buf-hist))))

(defvar helm-source-eww-history
  '((name . "eww history")
    (candidates . helm-eww-history-candidates)
    (migemo)
    (action . helm-eww-history-browse)))

(defvaralias 'anything-c-source-eww-history 'helm-source-eww-history)

(defun helm-eww-history ()
  (interactive)
  (helm :sources 'helm-source-eww-history
        :buffer "*helm eww*"))

(bind-keys :map eww-mode-map
           ("H" . helm-eww-history)
           ("s" . helm-eww-history)
           )

;; 情報源（helm-source-eww-history）を
;; M-x helm-for-files からでもアクセスできるようにする
;; （helm-for-files-preferred-list に入れる）
;; (add-to-list 'helm-for-files-preferred-list
;;         'helm-source-eww-history)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eww-lnum.el
;;; http://rubikitch.com/2014/11/12/eww-lnum/
;; (with-eval-after-load "eww"
;;   (define-key eww-mode-map "f" 'eww-lnum-follow)
;;   (define-key eww-mode-map "F" 'eww-lnum-universal))
;; (defun eww-lnum-read-interactive--not-truncate-lines (&rest them)
;;   (let ((truncate-lines nil))
;;     (apply them)))
;; (advice-add 'eww-lnum-read-interactive :around
;;             'eww-lnum-read-interactive--not-truncate-line)
;;; 下の ace-link に乗り換えたほうがいいらしい

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ace-link.el
;;; http://rubikitch.com/2014/12/30/ace-link/
;;; http://rubikitch.com/2015/01/07/ace-link-2/
;;; 次のパッケージに対応 : EWW, org-mode, info, help
;;; 2ストロークでリンクをたどれるようになる

;; デフォルトの設定(参考)
;; (defun ace-link-setup-default ()
;;   "Setup the defualt shortcuts."
;;   (require 'info)
;;   (define-key Info-mode-map "o" 'ace-link-info)
;;   (require 'help-mode)
;;   (define-key help-mode-map "o" 'ace-link-help)
;;   (require 'eww)
;;   (define-key eww-link-keymap "o" 'ace-link-eww)
;;   (define-key eww-mode-map "o" 'ace-link-eww))

(use-package ace-link
  :disabled t
  :ensure t
  :config
  (ace-link-setup-default)
  (require 'org)
  (define-key org-mode-map (kbd "C-c M-o") 'ace-link-org)

  ;;    text-property-any
  ;; -> next-single-property-change
  ;; にしないと一部のリンクが辿れないので再定義
  (defun ali--eww-collect-references ()
    "Collect the positions of visible links in the current `eww' buffer."
    (save-excursion
      (save-restriction
        (narrow-to-region
         (window-start)
         (window-end))
        (goto-char (point-min))
        (let ((skip (next-single-property-change (point) 'help-echo))
              candidates)
          (while (setq skip (text-property-not-all
                             skip (point-max) 'help-echo nil))
            (goto-char skip)
            (push skip candidates)
            (setq skip (next-single-property-change (point) 'help-echo)))
          (nreverse candidates)))))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 画像を非表示にする設定
;;; http://rubikitch.com/2014/11/25/eww-image/
;;; 非表示 : M-x eww-disable-images
;;; 再表示 : M-x eww-enable-images
;;;
;;; そのうちトグルを作ってもいいかもしれない

(defun eww-disable-images ()
  "ewwで画像表示させない"
  (interactive)
  (setq-local shr-put-image-function 'shr-put-image-alt)
  (eww-reload))

(defun eww-enable-images ()
  "ewwで画像表示させる"
  (interactive)
  (setq-local shr-put-image-function 'shr-put-image)
  (eww-reload))

(defun shr-put-image-alt (spec alt &optional flags)
  (insert alt))

;; デフォルトで非表示にする設定
(defun eww-mode-hook--disable-image ()
  (setq-local shr-put-image-function 'shr-put-image-alt))
(add-hook 'eww-mode-hook 'eww-mode-hook--disable-image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 辞書（Weblio, Wikipedia）設定（を追加する予定）
;;; http://rubikitch.com/2014/11/20/eww-weblio/


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ブラウジング時の配色を見やすくなるように設定（うまく動かない）
;;; http://rubikitch.com/2014/11/19/eww-nocolor/
;;; 変更する : M-x eww-enable-color
;;; 元に戻す : M-x eww-disable-color

;; うまく動作しなかったので、コメントアウトしておく <2015-01-10 19:20:20>
;; (defvar eww-disable-colorize t)
;; (defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
;;   (unless eww-disable-colorize
;;     (funcall orig start end fg)))
;; (advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
;; (advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)

;; (defun eww-disable-color ()
;;   "ewwで文字色を反映させない"
;;   (interactive)
;;   (setq-local eww-disable-colorize t)
;;   (eww-reload))

;; (defun eww-enable-color ()
;;   "ewwで文字色を反映させる"
;;   (interactive)
;;   (setq-local eww-disable-colorize nil)
;;   (eww-reload))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http://ergoemacs.org/emacs/emacs_eww_web_browser.html
;;; http://futurismo.biz/archives/2950
(defun eww-mode-hook--rename-buffer ()
  "Rename eww browser's buffer so sites open in new page."
  (rename-buffer "eww" t))
(add-hook 'eww-mode-hook 'eww-mode-hook--rename-buffer)

(provide 'my-eww)
;;; my-eww.el ends here

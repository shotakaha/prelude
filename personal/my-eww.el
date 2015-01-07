;;; my-eww.el -- Shota's configuration for EWW

;;; Commentary:
;;
;; EWWを使いやすくするための設定たち
;;
;; 1. EWWの履歴をhelm/anythingする設定
;; 2. eww-lnum の設定 → 次の ace-link に乗り換え
;; 3. ace-link の設定

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-eww.el
;;; EWWの履歴をhelm/anythingする方法
;;; http://rubikitch.com/2014/11/26/helm-eww/
;;; H, s で履歴の表示

(require 'eww)
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

(define-key eww-mode-map (kbd "H") 'helm-eww-history)
(define-key eww-mode-map (kbd "s") 'helm-eww-history)

;; 情報源（helm-source-eww-history）を
;; M-x helm-for-files からでもアクセスできるようにする
;; （helm-for-files-preferred-list に入れる）
(setq helm-for-files-preferred-list
      '(helm-source-buffers-list
        helm-source-recentf
        helm-source-bookmarks
        helm-source-file-cache
        helm-source-files-in-current-dir
        helm-source-eww-history
        helm-source-locate))

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
(prelude-require-package 'ace-link)

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

(provide 'my-eww)
;;; my-eww.el ends here

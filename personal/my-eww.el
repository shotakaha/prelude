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

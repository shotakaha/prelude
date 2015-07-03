;;; my-magit.el --- Magit configuration

;;; Commentary:

;; [2015-07-04] : Magitの設定をmy-editorから移植

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit.el
;;; http://rubikitch.com/2015/01/30/magit-time-format/
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :init
  :config
  ;; (setq magit-last-seen-setup-instructions "1.4.0")
  ;; (defun magit-format-duration--format-date (duration spec width)
  ;;   (format-time-string "%y-%m-%dT%H:%M:%S"
  ;;                       (seconds-to-time (- (float-time) duration))))
  ;; (advice-add 'magit-format-duration :override
  ;;             'magit-format-duration--format-date)
  ;; (defun magit-log-margin-set-timeunit-width--fixed ()
  ;;   (setq magit-log-margin-timeunit-width 12))
  ;; (advice-add 'magit-log-margin-set-timeunit-width :override
  ;;             'magit-log-margin-set-timeunit-width--fixed)
  ;; (setq magit-log-margin-spec '(33 nil magit-duration-spec))

  )

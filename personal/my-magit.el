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
  )

;;; my-magit.el --- Magit configuration

;;; Commentary:

;; [2015-07-04] : Magitの設定をmy-editorから移植

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit.el
;;; http://rubikitch.com/2015/01/30/magit-time-format/
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)
         )
  :init
  :config
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-gitflow.el
;;; まず gitflow をインストールしておく
;;; $ port install git-flow
(use-package magit-gitflow
  :ensure t
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; git-gutter
;;; https://github.com/syohex/emacs-git-gutter
;;; なんだかMagitの機能でも、追加／削除の箇所は分かる気がする。
;;; それに git-gutter/git-gutter+,
;;; git-gutter-fringe/git-gutter-fringe+ の使い分け方がよく分からないので
;;; とりあえずdisabled t とパッケージの削除をした
(use-package git-gutter
  :disabled t
  :bind ( ("C-x C-g" . git-gutter:toggle)
          ("C-x v =" . git-gutter:popup-hunk)
          ;; Jump to next/previous hunk
          ("C-x p" . git-gutter:previous-hunk)
          ("C-x n" . git-gutter:next-hunk)
          ;; Stage current hunk
          ("C-x v s" . git-gutter:stage-hunk)
          ;; Revert current hunk
          ("C-x v r" . git-gutter:revert-hunk)
          )
  :config
  ;; If you enable global minor mode
  (global-git-gutter-mode t)
  ;; If you would like to use git-gutter.el and linum-mode
  (git-gutter:linum-setup)
  ;; If you enable git-gutter-mode for some modes
  (add-hook 'ruby-mode-hook 'git-gutter-mode)

  ;; You can change the signs and those faces.
  (custom-set-variables
   '(git-gutter:modified-sign "  ") ;; two space
   '(git-gutter:added-sign "++")    ;; multiple character is OK
   '(git-gutter:deleted-sign "--"))

  (set-face-background 'git-gutter:modified "purple") ;; background color
  (set-face-foreground 'git-gutter:added "green")
  (set-face-foreground 'git-gutter:deleted "red")

  ;; You can change minor-mode-name in mode-line to set git-gutter:lighter. Default is " GitGutter"
  ;; first character should be a space
  (custom-set-variables
   '(git-gutter:lighter " GG"))
  )


(provide 'my-magit)
;;; my-magit.el ends here

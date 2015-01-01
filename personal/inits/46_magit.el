;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit.el
(use-package magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; git-commit.el
;;; git-commit-mode.el
;;; git-commit-training-wheels-mode.el
;;; Helps you craft well formed commit messages with git-commit-mode
;;; Directives for what makes a well formed commit come from tpope:
;;; http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
;;; Based in part on https://github.com/re5et/magit-commit-training-wheels
(use-package git-commit-training-wheels-mode
  :config
  (add-hook 'git-commit-mode-hook 'git-commit-training-wheels-mode)
  )
 ;; Not necessary if using ELPA package
;;; 上記のGitHubの usage は以下のように書いてあるのだが、
;;; magit-commit-training-wheels というパッケージがない
;; (require 'magit-commit-training-wheels)
;; (ad-activate 'magit-log-edit-commit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; git-gutter.el
;;; git-gutter+.el
;;; https://github.com/syohex/emacs-git-gutter
(use-package git-gutter
  :config
  (global-git-gutter-mode t)
  (global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
  (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)

  ;; Jump to next/previous hunk
  (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-x n") 'git-gutter:next-hunk)
  
  ;; Stage current hunk
  (global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
  
  ;; Revert current hunk
  (global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
  
  (custom-set-variables
   '(git-gutter:modified-sign "  ") ;; two space
   '(git-gutter:added-sign "++")    ;; multiple character is OK
   '(git-gutter:deleted-sign "--")
   '(git-gutter:lighter " GG")  ;; first character should be a space
   '(git-gutter:window-width 2)
   ;; '(setq git-gutter:modified-sign "⇔")
   ;; '(setq git-gutter:added-sign "⇒")
   ;; '(setq git-gutter:deleted-sign "⇐")
   )
  
  (set-face-foreground 'git-gutter:added  "green")
  (set-face-foreground 'git-gutter:deleted  "yellow")
  (set-face-background 'git-gutter:modified "purple")
  
)

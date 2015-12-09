;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-swoop.el
;;; http://rubikitch.com/2014/12/25/helm-swoop/
;;; 1. migemo の設定 (ローマ字で日本語検索できる) --> my-migemo.el (2015-10-10)
;;; 2. helm-migemo の設定
;;; 3. helm-swoop の設定
;;; 4. ace-search の設定
;;; 5. isearch-dabbrev の設定 （検索語の自動補完）

(add-to-list 'helm-for-files-preferred-list
             'helm-source-bookmark-set t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-c-yasnippet
(use-package helm-c-yasnippet
  :ensure t
  :bind (:map yas-minor-mode-map
              ("C-x i y" . helm-yas-complete)
              )
  :config
  (setq helm-yas-space-match-any-greedy t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-migemo.el
;;; http://rubikitch.com/2014/12/19/helm-migemo/
;;; この設定は不要らしい(disabled t)
;;; http://syohex.hatenablog.com/entry/2015/10/10/171926
(use-package helm-migemo
  :disabled t
  :ensure t
  :config
  ;; この修正が必要
  (eval-after-load "helm-migemo"
    '(defun helm-compile-source--candidates-in-buffer (source)
       (helm-aif (assoc 'candidates-in-buffer source)
           (append source
                   `((candidates
                      . ,(or (cdr it)
                             (lambda ()
                               ;; Do not use `source' because other plugins
                               ;; (such as helm-migemo) may change it
                               (helm-candidates-in-buffer (helm-get-current-source)))))
                     (volatile) (match identity)))
         source)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-swoop.el
;;; http://rubikitch.com/2014/12/25/helm-swoop/
;;; https://github.com/ShingoFukuyama/helm-swoop
(use-package helm-swoop
  :ensure t
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all)
         )
  :config
  (bind-keys :map isearch-mode-map
             ("M-i" . helm-swoop-from-isearch)
             )
  (bind-keys :map helm-swoop-map
             ("M-i" . helm-multi-swoop-all-from-helm-swoop)
             ("M-m" . helm-multi-swoop-current-mode-from-helm-swoop)
             ("C-r" . helm-previous-line)
             ("C-s" . helm-next-line)
             )
  (bind-keys :map helm-multi-swoop-map
             ("C-r" . helm-previous-line)
             ("C-s" . helm-next-line)
             )

  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t)
  ;; If this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows nil)
  ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
  (setq helm-swoop-split-direction 'split-window-vertically)
  ;; If nil, you can slightly boost invoke speed in exchange for text color
  (setq helm-swoop-speed-or-color nil)
  ;; ;; Go to the opposite side of line from the end or beginning of line
  (setq helm-swoop-move-to-line-cycle t)
  ;; Optional face for line numbers
  ;; Face name is `helm-swoop-line-number-face`
  (setq helm-swoop-use-line-number-face t)

  ;; Match/Search methods (Fuzzy matching, Migemo)
  ;; If you do not preferr fuzzy, remove it from the list below
  (defvar helm-c-source-swoop-match-functions
    '(helm-mm-exact-match
      helm-mm-match
      helm-fuzzy-match
      helm-mm-3-migemo-match))
  (setq helm-c-source-swoop-search-functions
        '(helm-mm-exact-search
          helm-mm-search
          helm-candidates-in-buffer-search-default-fn
          helm-fuzzy-search
          helm-mm-3-migemo-search))

  ;; In addition of above, you need to enable migemo mode if you'd like to
  (helm-migemo-mode 1)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-bm
;;; https://github.com/yasuyk/helm-bm
;;; http://rubikitch.com/2014/11/22/helm-bm/
(use-package helm-bm
  :ensure t
  :bind (("S-M-SPC" . bm-toggle-or-helm))
  :config

  (defun bm-toggle-or-helm ()
    "2回連続で起動したらhelm-bmを実行させる"
    (interactive)
    (bm-toggle)
    (when (eq last-command 'bm-toggle-or-helm)
      (helm-bm)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ace-isearch.el
;;; http://rubikitch.com/2014/10/08/ace-isearch/
(use-package ace-isearch
  :disabled t
  :ensure t
  :config
  (global-ace-isearch-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; isearch-dabbrev.el
;;; http://rubikitch.com/2014/12/23/isearch-dabbrev/
(use-package isearch-dabbrev
  :ensure t
  :init
  (bind-keys :map isearch-mode-map
             ("<tab>" . isearch-dabbrev-expand)
             )
  )

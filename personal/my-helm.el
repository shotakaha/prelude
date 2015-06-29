;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-swoop.el
;;; http://rubikitch.com/2014/12/25/helm-swoop/
;;; 1. migemo の設定 (ローマ字で日本語検索できる)
;;; 2. helm-migemo の設定
;;; 3. helm-swoop の設定
;;; 4. ace-search の設定
;;; 5. isearch-dabbrev の設定 （検索語の自動補完）

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; migemo.el
;;; http://rubikitch.com/2014/08/20/migemo/
;;; First, get cmigemo from http://www.kaoriya.net/software/cmigemo/
;;; See http://weblog.ymt2.net/blog/html/2013/08/23/install_migemo_to_emacs_24_3_1.html
(use-package migemo
  :ensure t
  :config
  (setq migemo-command "/usr/local/bin/cmigemo")
  (setq migemo-options '("-q" "--emacs"))

  ;; Set your installed path
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-migemo.el
;;; http://rubikitch.com/2014/12/19/helm-migemo/
(use-package helm-migemo
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
(use-package helm-swoop
  :ensure t
  :init
  ;; isearchからの連携を考えるとC-r/C-sにも割り当て推奨
  ;; (bind-keys :map helm-swoop-map
  ;;            ("C-r" . helm-previous-line)
  ;;            ("C-s" . helm-next-line)
  ;;            )
  :config
  ;; 検索結果をcycleしない、お好みで
  (setq helm-swoop-move-to-line-cycle nil)

  (cl-defun helm-swoop-nomigemo (&key $query ($multiline current-prefix-arg))
    "シンボル検索用Migemo無効版helm-swoop"
    (interactive)
    (let ((helm-swoop-pre-input-function
           (lambda () (format "\\_<%s\\_> " (thing-at-point 'symbol)))))
      (helm-swoop :$source (delete '(migemo) (copy-sequence (helm-c-source-swoop)))
                  :$query $query :$multiline $multiline)))
  ;; C-M-:に割り当て
  (bind-key "C-M-:" 'helm-swoop-nomigemo)

  ;; [2014-11-25 Tue]
  (when (featurep 'helm-anything)
    (defadvice helm-resume (around helm-swoop-resume activate)
      "helm-anything-resumeで復元できないのでその場合に限定して無効化"
      ad-do-it))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ace-isearch.el
;;; http://rubikitch.com/2014/10/08/ace-isearch/
(use-package ace-isearch
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

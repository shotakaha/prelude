;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm.el
(use-package helm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-descbinds.el
(use-package helm-descbinds
  :config
  (helm-descbinds-mode)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-ls-git.el
;;; 特に設定はいらない？

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-anything
(use-package helm-anything
  :disabled t
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-migemo
;;; http://rubikitch.com/2014/12/19/helm-migemo/

(use-package helm-migemo
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

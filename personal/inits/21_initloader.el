;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-loader.el
;; (defun init-loader-re-load (re dir &optional sort)
;;   (let ((load-path (cons dir load-path)))
;;     (dolist (el (init-loader--re-load-files re dir sort))
;;       (condition-case e
;;           (let ((time (car (benchmark-run (load (file-name-sans-extension el))))))
;;             (init-loader-log (format "loaded %s. %s" (locate-library el) time)))
;;         (error
;;          (init-loader-error-log (format "error %s. %s" (locate-library el) (error-message-string e))) ;追加
;;          )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; エラーが出た時だけ、ログを表示する
;; (custom-set-variables
;;  '(init-loader-show-log-after-init 'error-only))

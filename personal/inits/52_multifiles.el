;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; multifiles.el --- mark words in multiple files and edit in one buffer
(use-package multifiles
  :config
  (global-set-key (kbd "C-!") 'mf/mirror-region-in-multifile)

  (defadvice mf/mirror-region-in-multifile (before no-region (s e &optional buffer) activate)
    "regionを指定していない場合はバッファ全体を対象にする"
    (unless (region-active-p)
      (setq s (point-min) e (point-max))))
)

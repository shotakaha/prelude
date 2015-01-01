;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pandoc
;;; haskell-base の文書変換コマンド
;;; Emacsオリジナルではない
;;; ---
;;; ref : http://rubikitch.com/2014/09/22/pandoc/
;;; ---
(defun toorg(url)
  (interactive "sConvert site to org (URL):")
  (switch-to-buffer (format "*toorg: %s" url))
  (buffer-disable-undo)
  (erase-buffer)
  (shell-command
   (format "curl -s %s | pandoc -f html -t org" (shell-quote-argument url))
   (current-buffer))
  (save-excursion
    (while (re-search-forward "\\\\\\\\$" nil t)
      (replace-match "")))
  (org-mode)
  (view-mode 1))


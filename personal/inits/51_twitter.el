;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; twittering-mode.el
;;; http://www.emacswiki.org/emacs-ja/TwitteringMode-ja
(require 'twittering-mode)

;;; Edit key-bind
(add-hook 'twittering-mode-hook
          (lambda ()
            (mapc (lambda (pair)
                    (let ((key (car pair))
                          (func (cdr pair)))
                      (define-key twittering-mode-map
                        (read-kbd-macro key) func)))
                  '(("F" . twittering-friends-timeline)
                    ("R" . twittering-replies-timeline)
                    ("U" . twittering-user-timeline)
                    ("W" . twittering-update-status-interactive)))))

(setq twittering-icon-mode t)
(setq twittering-display-remaining t)

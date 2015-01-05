;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Key-bindings
;;; use bind-key instead of global-set-key
;;; see http://rubikitch.com/2014/09/10/bind-key/ for detail
(require 'bind-key)
(bind-key "C-h" 'delete-backward-char)
(bind-key "M-z" 'goto-line)
(bind-key "M-ESC ESC" 'keyboard-quit)
(bind-key "C-x +" 'follow-delete-other-windows-and-split)
;; (global-set-key (kbd "C-c c") '(lambda() (interactive) (compile "make -k")))
;; (global-set-key (kbd "C-c C") '(lambda() (interactive) (compile "make clean")))
;; (global-set-key (kbd "C-M-p") '(lambda () (interactive) (other-window -1)))  ;; C-x o
;; (global-set-key (kbd "C-M-n") '(lambda () (interactive) (other-window 1)))   ;; C-x o
(bind-key "C-c C-l" 'toggle-truncate-lines)
;; (bind-key "C-c C-j" 'open-junk-file)
(bind-key* "C-c C-m" 'org-capture)
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c :" 'ace-jump-buffer)
(bind-key "C-x :" 'anything-for-files)
(bind-key "M-y" 'anything-show-kill-ring)
(bind-key "C-x a s" 'auto-save-buffers-enhanced-toggle-activity)
(bind-key "M-;" 'comment-dwim-2)
;; (bind-key "C-x C-f" 'helm-find-files)
(bind-key [f12] 'eval-buffer)
(bind-key "C-x C-j" 'direx:jump-to-directory)
(bind-key "C-\\" 'direx-project:jump-to-project-root-other-window)
(bind-key "g" 'dired-k dired-mode-map)
;;(bind-key "K" 'direx-k direx:direx-modemap)
;; (bind-keys :map dired-mode-map
;;            ("o" . dired-omit-mode)
;;            ("a" . some-custom-dired-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C-x 3 C-x o --> C-t
;;; copied from http://d.hatena.ne.jp/rubikitch/20100210/emacs
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(bind-key* "C-t" 'other-window-or-split)
;;(bind-key* "C-u C-t" 'delete-other-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; key-chord.el
;; (use-package key-chord
;;   :config
;;   (key-chord-mode 1)
;;   (key-chord-define-global "uu"     'undo)
;;   (key-chord-define-global ",."     "<>\C-b")
;;   (key-chord-define-global "kj" 'view-mode)
;;   ;;(key-chord-define-global "up" 'package-list-packages)

;;   ;; Examples for TWO-key
;;   ;; (key-chord-define-global ",."     "<>\C-b")
;;   ;; (key-chord-define-global "hj"     'undo)
;;   ;; (key-chord-define-global [?h ?j]  'undo)  ; the same
;;   ;; (key-chord-define-global "jk"     'dabbrev-expand)
;;   ;; (key-chord-define-global "cv"     'reindent-then-newline-and-indent)
;;   ;; (key-chord-define-global "4r"     "$")

;;   ;; Examples for ONE-key
;;   ;; (key-chord-define-global "''"     "`'\C-b")
;;   ;; (key-chord-define-global ",,"     'indent-for-comment)
;;   ;; (key-chord-define-global "qq"     "the ")
;;   ;; (key-chord-define-global "QQ"     "The ")

;;   ;; Examples for Mode specific chords
;;   ;; (key-chord-define c++-mode-map ";;"  "\C-e;")
;;   ;; (key-chord-define c++-mode-map "{}"  "{\n\n}\C-p\t")
;;   )

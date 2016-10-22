;;; my-quickrun.el --- my quickrun settings

;;; Commentary:
;; IPNSウェブを生成するためのコマンドを quickrun に登録してみた
;;

;;; Code;


(use-package quickrun
  :ensure t
  :config
  (quickrun-add-command "ipns-hugo/alpha"
                        '((:command . "make")
                          (:exec    . ("make alpha"))
                          (:default-directory . "~/repos/kuma/ipns-hugo/")))
  (quickrun-add-command "ipns-hugo/beta"
                        '((:command . "make")
                          (:exec    . ("make beta"))
                          (:default-directory . "~/repos/kuma/ipns-hugo/")))
  (quickrun-add-command "ipns-hugo/dev"
                        '((:command . "make")
                          (:exec    . ("make dev"))
                          (:default-directory . "~/repos/kuma/ipns-hugo/")))
  (quickrun-add-command "ipns-hugo/pub"
                        '((:command . "make")
                          (:exec    . ("make public"))
                          (:default-directory . "~/repos/kuma/ipns-hugo/")))
  (quickrun-add-command "ipns-hugo/i18n"
                        '((:command . "make")
                          (:exec    . ("make alpha-i18n"))
                          (:default-directory . "~/repos/kuma/ipns-hugo/")))
  (quickrun-add-command "ipns-hugo/docs"
                        '((:command . "make")
                          (:exec    . ("make html"))
                          (:default-directory . "~/repos/kuma/ipns-hugo/docs/")))
  (quickrun-add-command "kumawatch/docs"
                        '((:command . "make")
                          (:exec    . ("make html"))
                          (:default-directory . "~/repos/kuma/kumawatch/docs/")))
  )

(provide 'my-quickrun)
;;; my-quickrun.el ends here

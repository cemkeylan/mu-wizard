;;; mu4e-config.el -- Configuration Options for mu4e -*- lexical-binding: t -*-

;;; Commentary:
;; This is a template configuration for mu4e, the basic stuff.  This can be
;; called from the init.el by running
;;
;;     (load-file "~/.config/mu4e/mu4e-config.el")
;;
;; My knowledge of Lisp isn't great.  I have been using Emacs for a few months
;; and this is my first project including Lisp.  So please bear with me.  I
;; would be happy to receive PRs.

;;; Code:
(require 'mu4e)

(defvar mu4e-get-mail-command)
(defvar mu4e-contexts)
(defvar mu4e-sent-messages-behavior)

(setq mu4e-get-mail-command       "mbsync -a"
      mail-user-agent             'mu4e-user-agent
      message-send-mail-function  'message-send-mail-with-sendmail
      sendmail-program            (executable-find "msmtp"))


;; Make mu4e-contexts an empty list.
(setq mu4e-contexts ())

;; Load user accounts.
(dolist (file (directory-files
               (expand-file-name "mu4e/accounts" (or (getenv "XDG_CONFIG_HOME") "~/.config")) t "\.el$" nil))
  (load file))

(provide 'mu4e-config)
;;; mu4e-config.el ends here

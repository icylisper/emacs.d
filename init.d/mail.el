(el-get-bundle mu4e)

(use-package mu4e
  :config
  (require 'org-mu4e)
  (setq mu4e-headers-skip-duplicates t
	mu4e-headers-leave-behavior 'apply
	mu4e-headers-auto-update t
	mu4e-headers-fields '((:human-date .  12)
			      (:from       .  24)
			      (:flags . 5)
			      (:subject . nil)))
  (setq mu4e-view-show-addresses t
	mu4e-view-show-images nil
	mu4e-view-prefer-html nil)

  (setq mu4e-index-cleanup t
	mu4e-index-lazy-check nil
	mu4e-hide-index-messages t)

  (setq mu4e-completing-read-function 'completing-read
	message-kill-buffer-on-exit t
	mu4e-context-policy 'pick-first
	mu4e-confirm-quit nil
	mu4e-change-filenames-when-moving t
	mu4e-split-view 'horizontal
	mu4e-sent-messages-behavior 'delete
	mu4e-use-fancy-chars nil
	org-mu4e-convert-to-html nil
	mu4e-html2text-command 'mu4e-shr2text)

  (setq mu4e-compose-signature-auto-include t)

  (add-hook 'mu4e-compose-mode-hook
	    (lambda ()
	      (set-fill-column 72)
	      (flyspell-mode)))

  (setq mu4e-maildir-shortcuts
	'(("/INBOX"      . ?i)
          ("/Sent" . ?s)
          ("/Trash"      . ?t)
          ("/Drafts"     . ?d))))

(use-package smtpmail
  :config
  (setq smtpmail-debug-info t))

(use-package message
  :config
  (setq message-fill-column 72
	message-send-mail-function 'smtpmail-send-it
	message-from-style 'angles
	message-citation-line-function 'message-insert-citation-line
	message-kill-buffer-on-exit t
        message-yank-prefix "> "
        message-yank-cited-prefix "> "
        message-yank-empty-prefix "> "
	mm-sign-option 'guided))

(el-get-bundle  davep/thinks.el
  :name thinks)
(use-package thinks)

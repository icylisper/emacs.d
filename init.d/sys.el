
(el-get-bundle ssh)
(el-get-bundle ssh-config)
(use-package ssh
  :init
  (add-hook 'ssh-mode-hook
	    (lambda ()
	      (setq ssh-directory-tracking-mode t)
	      (shell-dirtrack-mode t)
	      (setq dirtrackp nil))))

(use-package tramp
  :config
  (setq tramp-default-method "ssh"
	tramp-auto-save-directory "~/.emacs.d/var/tramp"
	password-cache-expiry 3600
	buffer-auto-save-file-name nil
	tramp-completion-reread-directory-timeout nil)
  (progn
    (add-to-list 'tramp-default-proxies-alist
		 '(nil "\\`root\\'" "/ssh:%h:"))
    (add-to-list 'tramp-default-proxies-alist
		 '((regexp-quote (system-name)) nil nil))))

(add-to-list 'exec-path "/usr/local/bin")
(use-package shell
  :config
  (progn
    (setq explicit-shell-file-name "bash")))

(use-package comint
  :defer t
  :config
  (progn
    (setf comint-prompt-read-only t
	  comint-use-prompt-regexp nil
          comint-history-isearch nil)
    (add-hook 'shell-mode-hook
              (lambda ()
		(define-key shell-mode-map (kbd "C-l") 'comint-clear-buffer)
		(define-key shell-mode-map
		  (kbd "C-r")
		  'comint-history-isearch-backward-regexp)))))

(el-get-bundle bash-completion)
(use-package bash-completion
  :disabled t
  :init
  (bash-completion-setup))

(el-get-bundle shell-command+)
(use-package shell-command+)

(el-get-bundle ieure/shell-here
  :name shell-here)
(use-package shell-here)

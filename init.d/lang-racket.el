(el-get-bundle racket-mode
  :depends (pos-tip))

(use-package racket-mode
  :config
    (add-hook 'racket-mode-hook
        (lambda ()
          (push '("lambda" . ?λ) prettify-symbols-alist)))
    (setq racket-program "/usr/local/bin/racket"
	  racket-images-inline t
	  tab-always-indent 'complete
	  comint-prompt-read-only t)
    (add-hook 'racket-mode-hook #'enable-paredit-mode)
    (add-hook 'racket-repl-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook 'paredit-mode)

    (add-hook 'racket-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'racket-repl-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode))

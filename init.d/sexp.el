(el-get-bundle paredit)
(use-package paredit
  :bind (("M-]" . paredit-forward-slurp-sexp)
	 ("M-[" . paredit-backward-slurp-sexp)
	 ("M-}" . paredit-forward-barf-sexp)
	 ("M-{" . paredit-backward-barf-sexp))
  :init
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package show-paren-mode
  :config
  (show-paren-mode 1)
  (setq show-paren-delay 0))
(show-paren-mode 1)

(el-get-bundle rainbow-delimiters)
(use-package rainbow-delimiters
  :init
  (require 'rainbow-delimiters nil)
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

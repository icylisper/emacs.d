(use-package lisp-mode
  :mode ("\\.lisp$" "\\.cl$" "stumpwmrc"))

(el-get-bundle slime)
(use-package slime
  :commands (slime slime-lisp-mode-hook)
  :config
  (add-to-list 'slime-contribs 'slime-fancy)
  (slime-setup '(slime-asdf slime-banner slime-fuzzy))
  (add-hook 'lisp-mode-hook 'slime-mode)
  (add-hook 'slime-repl-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (setq inferior-lisp-program "/usr/local/bin/sbcl --dynamic-space-size 1024"
	slime-net-encoding-system 'utf-8-unix
	slime-complete-symbol-function 'slime-fuzzy-complete-symbol
	slime-startup-animation t))

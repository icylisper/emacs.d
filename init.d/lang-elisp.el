;; elisp

(use-package elisp-mode
  :config
  (progn
    (setq tab-always-indent 'complete)
    (add-to-list 'completion-styles 'initials t))
  :bind (("M-." . find-function-at-point)
         ("M-&" . complete-symbol))
  :interpreter (("emacs" . emacs-lisp-mode)))

(use-package eldoc
  :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))

(use-package ert
  :config
  (add-to-list 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords))

(use-package ielm
  :defer t
  :config
  (progn
    (add-hook 'ielm-mode-hook (lambda () (paredit-mode 1)))
    (define-key ielm-map (kbd "C-c C-z") #'quit-window)))

(el-get-bundle wdired)
(use-package wdired
  :bind (:map dired-mode-map
	      ("r" . wdired-change-to-wdired-mode))
  :config
  (setq directory-sep-char ?/)
  (add-hook 'dired-load-hook
	    (lambda ()
	      (load "dired-x")
	      (setq directory-sep-char ?/
		    wdired-allow-to-change-permissions t
		    dired-backup-overwrite t)))
  (add-hook 'dired-mode-hook #'highline-mode-on))

(el-get-bundle dired-details)
(use-package dired-details
  :init
  (dired-details-install)
  :config
  (bind-key (kbd "C-x C-d") 'dired)
  (setq dired-details-hidden-string ""
	dired-dwim-target t))

(el-get-bundle dired-hacks)
(use-package dired-subtree
  :bind (:map dired-mode-map
	      ("<tab>" . dired-subtree-cycle)
	      ("i" . dired-subtree-insert)
	      ("k" . dired-subtree-remove))
  :config
  (setq dired-subtree-line-prefix
	(lambda (depth) (make-string (* 2 depth) ?\s)))
  (setq dired-subtree-use-backgrounds nil))

(defun dired-lynx-keybindings ()
  (define-key dired-mode-map [left]  'dired-up-directory)
  (define-key dired-mode-map [right] 'dired-view-file))
(add-hook 'dired-mode-hook 'dired-lynx-keybindings)
(add-hook 'dired-mode-hook #'highline-mode-on)

(el-get-bundle rust-mode)
(use-package rust-mode
  :mode ("\\.rs$" . rust-mode)
  :config
  (progn
    (setq rust-format-on-save nil)
    (add-hook 'rust-mode-hook
	      (lambda ()
		(setq prettify-symbols-alist
		      '(("fn" . 955)
			("->" . 8594)))))))

(add-to-list 'exec-path "~/.cargo/bin")
(el-get-bundle cargo)
(use-package cargo
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (setq compile-command "cargo build")
  :bind
  (:map rust-mode-map
	("C-c C-b" . cargo-process-build)
	("C-c C-k" . cargo-process-run)
	("C-c C-r" . cargo-process-run)))


(el-get-bundle rustic)
(el-get-bundle eglot)

(use-package rustic
  :config
  (progn
    ;; (setq rustic-lsp-client 'eglot
    ;; 	  rustic-lsp-server 'rust-analyzer
    ;; 	  lsp-rust-server 'rust-analyzer
    ;; 	  eglot-send-changes-idle-time (* 60 60))
    (setq rustic-lsp-client nil)
    (setq rustic-format-on-save nil
	  rustic-flycheck-setup-mode-line-p nil))

  (add-hook 'eglot-managed-mode-hook (lambda ()
				       (eldoc-mode -1)
				       (flymake-mode -1))))

(defun rustic-mode-auto-save-hook ()
  "Enable auto-saving in rustic-mode buffers."
  (when buffer-file-name
    (setq-local compilation-ask-about-save nil)))
(add-hook 'rustic-mode-hook 'rustic-mode-auto-save-hook)

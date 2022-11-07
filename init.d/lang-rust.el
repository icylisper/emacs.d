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

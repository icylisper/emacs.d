;; golang
(el-get-bundle go-mode)
(use-package go-mode
  :config
  (add-hook 'go-mode-hook
	    (lambda ()
	      (setq indent-tabs-mode 1)
	      (setq tab-width 4))))

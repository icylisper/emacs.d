;; janet

(el-get-bundle SerialDev/ijanet-mode :name ijanet-mode)
(use-package ijanet-mode)

(el-get-bundle ALSchwalm/janet-mode :name janet-mode)
(use-package janet-mode)

(el-get-bundle velkyel/inf-janet :name inf-janet)
(use-package inf-janet
  :config
  (setq inf-janet-program "~/lib/janet/bin/janet"))

(add-hook 'janet-mode-hook 'rainbow-delimiters-mode)
(add-hook 'janet-mode-hook 'paredit-mode)

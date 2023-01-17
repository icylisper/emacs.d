(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))

(setq js-indent-level 2)
(use-package js-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
  :config
  (setq js-indent-level 2))

(setq css-indent-offset 2)

(el-get-bundle leafOfTree/svelte-mode :name svelte-mode)
(use-package svelte-mode
  :config
  (setq svelte-basic-offset 2))

;; solidity
(el-get-bundle solidity-mode)
(use-package solidity-mode)

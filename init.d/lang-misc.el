(el-get-bundle yaml-mode)
(use-package yaml-mode)

;; markdown
(el-get-bundle markdown-mode)
(use-package markdown-mode)

;; graphql
(el-get-bundle davazp/graphql-mode :name graphql-mode)
(use-package graphql-mode)

(el-get-bundle any-ini-mode)
(use-package any-ini-mode
  :config
  (add-to-list 'auto-mode-alist '(".*\\.conf$" . any-ini-mode)))

;; toml-mode

(el-get-bundle dryman/toml-mode.el :name toml-mode)
(use-package toml-mode
  :config
  (add-to-list 'auto-mode-alist '(".*\\.toml$" . any-ini-mode)))

(el-get-bundle terraform-mode)
(use-package terraform-mode)

;; (el-get-bundle christophstockhusen/bigquery-mode
;;   :name elfeed-org)

(el-get-bundle protobuf-mode)
(use-package protobuf-mode)

(el-get-bundle xcezx/blockdiag-mode :blockdiag)
(use-package blockdiag)

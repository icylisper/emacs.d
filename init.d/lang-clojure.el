;; language modes
(el-get-bundle clojure-mode)
(el-get-bundle edn)
(use-package clojure-mode
  :mode ("\\.edn$" "\\.clj$" "\\.cljc$")
  :config
  (add-hook 'clojure-mode-hook
	    (lambda ()
	      (push '("fn" . ?λ) prettify-symbols-alist)))
  (setq clojure-align-forms-automatically t
	clojure-indent-style 'align-always
	comment-column 70))

(el-get-bundle vspinu/sesman :name sesman)
(el-get-bundle a)
(el-get-bundle cider :checkout "v1.2.0")
(el-get-bundle parseclj)

(use-package cider
  :config
  (setq nrepl-hide-special-buffers nil
	nrepl-buffer-name-show-port t
	nrepl-log-messages nil)
  (setq cider-repl-display-help-banner nil
	cider-repl-use-clojure-font-lock t
	cider-repl-tab-command #'indent-for-tab-command)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
  :bind
  (("C-c n"  . cider-find-ns)
   ("C-c e"  . cider-visit-error-buffer)
   ("C-c M-c". cider-connect)
   ("C-c r"  . cider-switch-to-repl-buffer)
   :map cider-repl-mode-map
   ("C-l"    . cider-repl-clear-buffer)
   ("M-p"    . cider-repl-previous-input)))

(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)


(el-get-bundle ericdallo/jet.el :name jet)

(use-package jet)

(defun jet-json-to-clipboard ()
  (interactive)
  (jet-to-clipboard (jet--thing-at-point) '("--from=json" "--to=edn")))

(global-set-key (kbd "C-c j j e") 'copy-json-as-edn)

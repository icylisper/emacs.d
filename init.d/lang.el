
(defun runtime-path (dir)
  (expand-file-name dir "~/runtime"))

(defun lsp-path (bin)
  (expand-file-name bin "~/runtime/emacs/lsp"))

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


;; common-lisp

(use-package lisp-mode
  :mode ("\\.lisp$" "\\.cl$" "stumpwmrc"))

(el-get-bundle slime)
(use-package slime
  :commands (slime slime-lisp-mode-hook)
  :config
  (add-to-list 'slime-contribs 'slime-fancy)
  (slime-setup '(slime-asdf slime-banner slime-fuzzy))
  (setq inferior-lisp-program "/usr/local/bin/sbcl --dynamic-space-size 1024"
	slime-net-encoding-system 'utf-8-unix
	slime-complete-symbol-function 'slime-fuzzy-complete-symbol
	slime-startup-animation t))


;; clojure

(add-to-list 'exec-path (runtime-path "clojure/bin"))
(setenv "JAVA_HOME" (runtime-path "java"))

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
(el-get-bundle cider :checkout "v1.6.0")
(el-get-bundle parseclj)

(use-package cider
  :config
  (setq nrepl-hide-special-buffers nil
	nrepl-buffer-name-show-port t
	nrepl-log-messages nil)
  (setq cider-preferred-build-tool 'clojure-cli)
  (setq cider-repl-display-help-banner nil
	cider-repl-use-clojure-font-lock t
	cider-repl-display-in-current-window nil
	cider-repl-buffer-size-limit 100000
	cider-repl-tab-command #'indent-for-tab-command
	cider-repl-use-pretty-printing nil
	cider-repl-use-content-types t
	cider-repl-wrap-history t)

  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
  :bind
  (("C-c M-c". cider-connect)
   :map cider-repl-mode-map
   ("<return>" . cider-repl-return)
   ("C-<return>" . cider-repl-newline-and-indent)
   ("C-l"  . cider-repl-clear-buffer)
   ("M-p"  . cider-repl-previous-input)))

(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

(add-hook 'cider-repl-mode-hook '(lambda () (setq scroll-conservatively 101)))

(el-get-bundle ericdallo/jet.el :name jet)
(use-package jet)

(defun jet-json-to-clipboard ()
  (interactive)
  (jet-to-clipboard (jet--thing-at-point) '("--from=json" "--to=edn")))

;; racket

(el-get-bundle racket-mode
  :depends (pos-tip))

(use-package racket-mode
  :config
    (add-hook 'racket-mode-hook
        (lambda ()
          (push '("lambda" . ?λ) prettify-symbols-alist)))
    (setq racket-program (runtime-path "racket/bin/racket")
	  racket-images-inline t
	  tab-always-indent 'complete
	  comint-prompt-read-only t))


;; janet

(el-get-bundle SerialDev/ijanet-mode :name ijanet-mode)
(use-package ijanet-mode)

(el-get-bundle ALSchwalm/janet-mode :name janet-mode)
(use-package janet-mode)

(el-get-bundle velkyel/inf-janet :name inf-janet)
(use-package inf-janet
  :config
  (setq inf-janet-program (runtime-path "janet/bin/janet")))


(add-hook 'janet-mode-hook 'paredit-mode)


(el-get-bundle paredit)
(use-package paredit
  :bind (("M-]" . paredit-forward-slurp-sexp)
	 ("M-[" . paredit-backward-slurp-sexp)
	 ("M-}" . paredit-forward-barf-sexp)
	 ("M-{" . paredit-backward-barf-sexp))
  :init
  (add-hook 'lisp-mode-hook 'slime-mode)
  (add-hook 'slime-repl-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'racket-mode-hook #'enable-paredit-mode)
  (add-hook 'racket-repl-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package show-paren-mode
  :config
  (show-paren-mode 1)
  (setq show-paren-delay 0))

(show-paren-mode 1)

(el-get-bundle rainbow-delimiters)
(use-package rainbow-delimiters
  :init
  (require 'rainbow-delimiters nil)
  :config
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'janet-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'racket-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'racket-repl-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode))


;; end of lisps

;; elixir

(el-get-bundle elixir)
(use-package elixir)


;; erlang

(setq load-path (cons (runtime-path "erlang/emacs") load-path))
(require 'erlang-start)
(setq erlang-root-dir (runtime-path "erlang"))
(setq exec-path (cons (runtime-path "erlang/bin") exec-path))
(setq erlang-man-root-dir (runtime-path "erlang/bin/man"))

(use-package erlang
  :mode (("\\.erl?$" . erlang-mode)
	 ("rebar\\.config$" . erlang-mode)
	 ("relx\\.config$" . erlang-mode)
	 ("sys\\.config\\.src$" . erlang-mode)
	 ("sys\\.config$" . erlang-mode)
	 ("\\.config\\.src?$" . erlang-mode)
	 ("\\.config\\.script?$" . erlang-mode)
	 ("\\.hrl?$" . erlang-mode)
	 ("\\.app?$" . erlang-mode)
	 ("\\.app.src?$" . erlang-mode)
	 ("\\Emakefile" . erlang-mode)))


;; go

(el-get-bundle go-mode)
(use-package go-mode
  :config
  (add-hook 'go-mode-hook
	    (lambda ()
	      (setq indent-tabs-mode 1
		    tab-width 4))))


;; javascript
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

;; python

(add-to-list 'exec-path (runtime-path "python/bin"))

(use-package python-mode
  :interpreter "python"
  :config
  (setq python-indent-offset 4))

;; ruby

(el-get-bundle inf-ruby)

(use-package ruby-mode
  :config
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

(el-get-bundle dgutov/robe :name robe)
(use-package robe)


;; rust
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


(el-get-bundle  jorgenschaefer/project-el :name project)
(el-get-bundle brotzeit/rustic :name rustic)

(use-package rustic
  :config
  (progn
    (setq lsp-rust-server 'rust-analyzer)
    (setq rustic-lsp-client 'eglot
	  rustic-analyzer-command '("/usr/local/bin/rust-analyzer")
	  rustic-lsp-server 'rust-analyzer)
    (setq rustic-format-on-save nil
	  rustic-flycheck-setup-mode-line-p nil)))


(defun rustic-mode-auto-save-hook ()
  "Enable auto-saving in rustic-mode buffers."
  (when buffer-file-name
    (setq-local compilation-ask-about-save nil)))

(add-hook 'rustic-mode-hook 'rustic-mode-auto-save-hook)


;; dirty formats

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

(el-get-bundle xcezx/blockdiag-mode :blockdiag)
(use-package blockdiag)

;; flutter
(el-get-bundle bradyt/dart-mode :name dart-mode)
(use-package dart-mode)

(el-get-bundle amake/flutter.el :name flutter)
(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload)))

;; lsp

(el-get-bundle eglot)
(el-get-bundle external-completion)

(use-package eglot
  :config
  (setq eglot-send-changes-idle-time (* 60 60))

  (add-hook 'eglot-managed-mode-hook (lambda ()
				       (eldoc-mode 1))))

(defun setup-workspaces ()
  (setq-local eglot-workspace-configuration
	      '(:rust-analyzer
		( :procMacro ( :attributes (:enable t)
			      :enable t)
		  :cargo (:buildScripts (:enable t))
		  :diagnostics (:disabled ["unresolved-proc-macro"
					   "unresolved-macro-call"])))))

(add-hook 'rust-mode-hook #'setup-workspaces)


(defclass eglot-analyzer (eglot-lsp-server) ()
  :documentation "A custom class for rust-analyzer.")

;; Rust-analyzer requires the workspaceConfiguration sent as
;; initializationOptions at startup time. See
(cl-defmethod eglot-initialization-options ((server eglot-analyzer))
  eglot-workspace-configuration)

(add-to-list 'eglot-server-programs
             '(rust-mode . (eglot-analyzer "rust-analyzer")))


;; (el-get-bundle flycheck)
;; (el-get-bundle intramurz/flycheck-eglot :name flycheck-eglot)


;; generic lang compile

(use-package compile
  :no-require
  :bind (("C-c c" . compile)
         ("M-O"   . show-compilation))
  :bind (:map compilation-mode-map
              ("z" . delete-window))
  :preface
  (defun show-compilation ()
    (interactive)
    (let ((it
           (catch 'found
             (dolist (buf (buffer-list))
               (when (string-match "\\*compilation\\*" (buffer-name buf))
                 (throw 'found buf))))))
      (if it
          (display-buffer it)
        (call-interactively 'compile))))

  (defun compilation-ansi-color-process-output ()
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start)
         (point-marker)))

  :hook (compilation-filter . compilation-ansi-color-process-output))

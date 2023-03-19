(package-initialize)

(defconst init-file (expand-file-name "emacs-init.el" user-emacs-directory)
  "All configurations are stored in this file.")

(require 'cl-lib)

(add-to-list 'load-path "~/lib/emacs/el-get/el-get")
(add-to-list 'load-path "~/lib/emacs/el-get")
(setq el-get-dir "~/lib/emacs/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; basic lib
(el-get-bundle use-package)
(el-get-bundle s)
(el-get-bundle f)
(el-get-bundle popup)
(el-get-bundle epc)
(el-get-bundle hydra)
(el-get-bundle tablist)
(el-get-bundle alert)
(el-get-bundle ctable)
(el-get-bundle bddean/xml-plus :name xml-plus)
(el-get-bundle esxml)
(el-get-bundle oauth2)
(el-get-bundle alphapapa/ts.el :name ts)
(el-get-bundle request)
(el-get-bundle jwiegley/emacs-async :name async)
(el-get-bundle pcre2el)

(package-initialize)

(use-package queue :ensure t)

(setq warning-minimum-level :emergency
      initial-scratch-message ";; happy hacking")
(el-get-bundle no-littering)
(use-package no-littering
  :init
  (require 'no-littering)
  :config
  (setq no-littering-etc-directory (expand-file-name "config/" user-emacs-directory)
	no-littering-var-directory (expand-file-name "data/" user-emacs-directory)))
(setq sentence-end-double-space nil)

;; backups
(setq backup-directory-alist '((".*" . "/tmp/"))
      auto-save-file-name-transforms '((".*" "/tmp/" t))
      backup-by-copying t
      confirm-nonexistent-file-or-buffer nil
      kept-new-versions 0
      kept-old-versions 0
      delete-old-versions t)

;; keyboard
(setq ns-function-modifier 'control
      x-select-enable-clipboard t
      echo-keystrokes 0.1)

;; no bells and whistles
(tooltip-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(mouse-wheel-mode t)
(blink-cursor-mode 0)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq frame-title-format '(buffer-file-name "%f" ("%b"))
      inhibit-startup-message t
      inhibit-startup-screen t
      ring-bell-function 'ignore
      use-dialog-box nil
      visible-bell nil)

;; disable mouse
(el-get-bundle purcell/disable-mouse :name disable-mouse)
(use-package disable-mouse
  :diminish disable-mouse-global-mode
  :delight disable-mouse-global-mode
  :config
  (global-disable-mouse-mode))

;; custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


(defun unfringe ()
  (interactive)
  (set-face-attribute 'fringe nil :background nil))

(set-face-attribute 'variable-pitch nil :font "inconsolata" :height 148)
(set-frame-font "hasklig 13" nil t)

;;themes
(add-to-list 'custom-theme-load-path "~/lib/emacs/themes")
(add-to-list 'load-path "~/lib/emacs/themes")

(defvar theme-hooks nil)

(defun disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defun add-theme-hook (theme-id hook-func)
  (add-to-list 'theme-hooks (cons theme-id hook-func)))

(defun load-theme-advice
    (f theme-id &optional no-confirm no-enable &rest args)
  (unless no-enable
    (disable-all-themes))
  (prog1
      (apply f theme-id no-confirm no-enable args)
    (unless no-enable
      (pcase (assq theme-id theme-hooks)
	(`(,_ . ,f) (funcall f))))))

(advice-add 'load-theme :around #'load-theme-advice)

;; modeline

(el-get-bundle smart-mode-line)
(use-package smart-mode-line
  :init
  (sml/setup)
  (setq sml/no-confirm-load-theme t
	sml/vc-mode-show-backend t
	;sml/mode-width 10
	;sml/name-width 20
	resize-mini-windows nil)
  (sml/apply-theme nil)
  :config
  (dolist (m '("AC" "Undo-Tree" "ARev" "Anzu" "Guide" "company"))
    (add-to-list 'sml/hidden-modes (concat " " m))))

(use-package time
  :config
  (display-time-mode)
  (setq
   display-time-day-and-date nil
   display-time-24hr-format t
   display-time-default-load-average nil))

(use-package battery
  :config
  (display-battery-mode))

;; window

(setq window-combination-resize t
      resize-mini-windows nil
      max-mini-window-height 1)

(el-get-bundle ace-window)
(use-package ace-window
  :config
  (bind-key "C-x o" 'ace-window))

(use-package windmove
  :config
  (windmove-default-keybindings 'shift)
  (setq windmove-wrap-around t)
  (bind-key [M-right] 'windmove-right)
  (bind-key [M-left]  'windmove-left)
  (bind-key [M-up] 'windmove-up)
  (bind-key [M-down]  'windmove-down))


;; buffer

(setq fill-column 80
      next-line-add-newlines nil
      require-final-newline nil
      truncate-partial-width-windows nil
      sentence-end-double-space nil
      indent-tabs-mode nil
      enable-local-variables nil)
(add-hook 'prog-mode-hook
	  (lambda ()
	    (font-lock-add-keywords
	     nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
		    1 font-lock-warning-face t)))))
(auto-revert-mode 1)
(add-to-list 'write-file-functions 'delete-trailing-whitespace)
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

(global-set-key "\C-xk" 'kill-this-buffer)

;; undo tree
(el-get-bundle akhayyat/emacs-undo-tree :name undo-tree)
(use-package undo-tree
  :init (global-undo-tree-mode)
  :config (setq
	   undo-tree-visualizer-diff t
	   undo-tree-visualizer-timestamps t))

;; goto line
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; highline
(el-get-bundle highline)
(use-package highline-mode
  :config
  (bind-key (kbd "C-h C-i") 'highline-mode)
  (defadvice list-buffers (after highlight-line activate)
  (save-excursion
    (set-buffer "*Buffer List*")
    (highline-mode-on))))

;; swap-buffers
(defun swap-buffers ()
  "Put the buffer from the selected window in next window, and vice versa."
  (interactive)
  (let* ((this (selected-window))
         (other (next-window))
         (this-buffer (window-buffer this))
         (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)))

(el-get-bundle ace-jump-mode)
(use-package ace-jump-mode
  :init
  (autoload 'ace-jum-mode "ace-jump-mode" "Emacs quick move" t)
  (bind-key (kbd "C-c i") 'ace-jump-mode))

;; anzu
(el-get-bundle anzu)
(use-package anzu
  :config
  (global-anzu-mode +1))

(el-get-bundle purcell/whole-line-or-region
  :name whole-line-or-region)
(use-package whole-line-or-region)

;; minibuffer
(use-package minibuffer
  :config
  (setq enable-recursive-minibuffers t
        resize-mini-windows nil)
  (file-name-shadow-mode)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))

(use-package imenu
  :config
  (setq imenu-auto-rescan t)
  (defun imenu--use-package ()
    (setq imenu-generic-expression
          '((nil
             "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2))))
  (add-hook 'emacs-lisp-mode-hook #'imenu--use-package)
  :bind
  (("C-c j" . imenu)))


(el-get-bundle bmag/imenu-list :name imenu-list)


(use-package imenu-list
  :commands imenu-list-minor-mode)

;; align

(use-package align
  :bind (("M-["   . align-code)
         ("C-c [" . align-regexp))
  :commands align
  :preface
  (defun align-code (beg end &optional arg)
    (interactive "rP")
    (if (null arg)
        (align beg end)
      (let ((end-mark (copy-marker end)))
        (indent-region beg end-mark nil)
        (align beg end-mark)))))


;; completion
(ido-mode -1)

(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-answer-short t
      completion-styles '(partial-completion substring)
      completion-ignore-case t
      completion-cycle-threshold 2
      completion-flex-nospace nil
      completion-pcm-complete-word-inserts-delimiters t
      completion-pcm-word-delimiters "-_./:| ")

;; completions buffer

;; styles:  completion-styles-alist
;; category defaults: completion-category-defaults
;; catch-all completion-styles

(setq completion-show-help nil
      completions-format 'vertical
      completion-category-overrides '((file (styles basic substring))
				      (buffer (styles initials flex)
					      (cycle . 3))))

;; completions buffer height

(defvar old-max-height-function temp-buffer-max-height)

(defun max-completions-height (buffer)
  (if (string= (buffer-name buffer) "*Completions*")
      16
    (funcall old-max-height-function temp-buffer-max-height)))

(setq temp-buffer-max-height #'max-completions-height)
(temp-buffer-resize-mode)


;; dired
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

;; lang
(defun runtime-path (dir)
  (expand-file-name dir "~/runtime"))

(defun lib-path (bin)
  (expand-file-name bin "~/lib"))

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

(el-get-bundle ericdallo/jet.el :name jet)
(use-package jet)

(defun jet-json-to-clipboard ()
  (interactive)
  (jet-to-clipboard (jet--thing-at-point) '("--from=json" "--to=edn")))

;; racket

(add-to-list 'exec-path (runtime-path "racket/bin"))

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

(add-to-list 'exec-path (runtime-path "janet/bin"))

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

(add-to-list 'exec-path (runtime-path "elixir/bin"))

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

(add-to-list 'exec-path (runtime-path "go/bin"))

(el-get-bundle go-mode)
(use-package go-mode
  :config
  (add-hook 'go-mode-hook
	    (lambda ()
	      (setq indent-tabs-mode 1
		    tab-width 4))))


;; ocaml
(add-to-list 'exec-path (runtime-path "ocaml/bin"))

(el-get-bundle tuareg-mode)
(use-package tuareg-mode
  :mode ("\\.ml[ily]?$" . tuareg-mode)
  :config
  (progn

    (dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi"))
      (add-to-list 'completion-ignored-extensions ext))

    (setq tuareg-indent-align-with-first-arg t
	  tuareg-comment-show-paren t
	  tuareg-match-patterns-aligned t)

    (add-hook 'tuareg-mode-hook
              (lambda()
		(when (functionp 'prettify-symbols-mode)
                  (prettify-symbols-mode)))))
  :bind
  (:map tuareg-mode-map
	("C-c C-c" . compile)))


;; javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))

(add-to-list 'exec-path (lib-path "node/bin"))

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

(add-to-list 'exec-path (lib-path "python/bin"))
(add-to-list 'exec-path (runtime-path "python/bin"))

(use-package python-mode
  :interpreter "python"
  :config
  (setq python-indent-offset 4))

;; ruby

(add-to-list 'exec-path (runtime-path "ruby/bin"))

(el-get-bundle inf-ruby)

(use-package ruby-mode
  :config
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

(el-get-bundle dgutov/robe :name robe)
(use-package robe)


;; rust

(setenv "RUSTUP_TOOLCHAIN" "stable")
(setenv "RUST_TARGET_DIR" (lib-path "rust/cargo/target"))

(add-to-list 'exec-path (lib-path "rust/cargo/bin"))
(el-get-bundle rust-mode)
(use-package rust-mode
  :mode ("\\.rs$" . rust-mode)
  :config
  (progn
    (electric-indent-mode 0)
    (setq rust-format-on-save nil)
    (add-hook 'rust-mode-hook
	      (lambda ()
		(setq indent-tabs-mode nil)
		(setq prettify-symbols-alist
		      '(("fn" . 955)
			("->" . 8594))))))
  :bind
  (:map rust-mode-map
	("C-c C-c" . rust-compile)
	("C-c C-l" . rust-run-clippy)
	("C-c C-d" . eldoc-print-current-symbol-info)))

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


;; flymake

(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))

(global-set-key (kbd "C-c n") 'flymake-goto-next-error)
(global-set-key (kbd "C-c p") 'flymake-goto-prev-error)


;; compilation

(use-package compile
  :no-require
  :bind (("C-c c" . compile))
  :bind (:map compilation-mode-map
              ("z" . delete-window))

  :config
  (setq compilation-read-command nil
	compile-command "make -k")
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


;; xref

(use-package xref
  :bind (("M-." . #'xref-find-definitions)
         ("M-;" . #'xref-go-back)
         ("M-r" . #'xref-find-references)))

;; lsp

(add-to-list 'exec-path (lib-path "lsp"))
(add-to-list 'exec-path (lib-path "lsp/elixir-ls"))

;;(el-get-bundle external-completion)

(use-package eglot
  :config
  (setq eglot-send-changes-idle-time (* 60 60))
  ;;(add-to-list 'eglot-stay-out-of 'flymake)
  (add-hook 'eglot-managed-mode-hook (lambda ()
				       (eldoc-mode 1)
				       (flymake-mode -1))))

(defclass eglot-rust-x-analyzer (eglot-lsp-server) ()
  :documentation "A custom class for rust-analyzer.")

(cl-defmethod eglot-initialization-options ((server eglot-rust-x-analyzer))
  '(:rust-analyzer
    ( :procMacro ( :attributes (:enable t)
		   :enable t)
      :cargo (:buildScripts (:enable t))
      :diagnostics (:disabled ["unresolved-proc-macro"
			       "unresolved-macro-call"]))))

(setq eglot-server-programs
      '((python-mode . ("pyls"))
	(clojure-mode . ("clojure-lsp"))
	(elixir-mode . ("language_server.sh"))
	(caml-mode . ("ocamllsp"))
	(erlang-mode . ("erlang_ls" "--transport" "stdio"))))

(add-to-list 'eglot-server-programs
             '(rust-mode . (eglot-rust-x-analyzer "rust-analyzer" "-v"
						  "--log-file" "/tmp/ra.log")))

(defun eglot-connect ()
  (interactive)
  (eglot-ensure))

;; tree-sitter

(use-package tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


;; project
(use-package vc
  :config
  (setq vc-mistrust-permissions t
	version-control t
        vc-initial-comment t
	vc-follow-symlinks nil
        vc-consult-headers nil
        vc-make-backup-files t))

(el-get-bundle sshaw/git-link :name git-link)
(use-package git-link)
(el-get-bundle dash)
(el-get-bundle magit/transient :name transient)
(el-get-bundle with-editor)

(add-to-list 'load-path "~/lib/emacs/el-get/with-editor/lisp")

(el-get-bundle transient)
(use-package transient
     :config
     (setq transient-hide-during-minibuffer-read t
	   transient-show-popup 0.1))

(el-get-bundle magit/magit
  :name magit
  :depends (dash transient with-editor)
  :pkgname "magit/magit"
  :load-path "lisp/"
  :compile "lisp/")

(use-package magit
  :init
  (progn
    (add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode)))
  :config
  (setq magit-auto-revert-mode 1
	magit-last-seen-setup-instructions "1.4.0"
	diff-switches "-u"
	magit-push-always-verify nil
	magit-git-executable "git"
	magit-save-repository-buffers 'dontask
	magit-default-tracking-name-function
	#'magit-default-tracking-name-branch-only)
  (add-hook 'magit-mode-hook #'highline-mode-on)
  (setq magit-repolist-columns
	'(("Name"       25  magit-repolist-column-ident nil)
          ("Branch"     10  magit-repolist-column-branch)
          ("Version"    25  magit-repolist-column-version nil)
          ("↓P"         5   magit-repolist-column-unpulled-from-pushremote)
          ("↑P"         5   magit-repolist-column-unpushed-to-pushremote)
          (""           6   magit-repolist-column-dirty)
          ("Path"       99  magit-repolist-column-path nil)))
  :bind
  (("C-c m" . magit-status)
   ("C-c l" . magit-log-buffer-file)
   ("C-c L" . magit-log-head)
   ("C-c o" . magit-checkout)
   ("C-c d" . magit-diff-buffer-file)
   ("C-c D" . magit-diff)))

(el-get-bundle magit-filenotify)
(use-package magit-filenotify
  :config
  (add-hook 'magit-status-mode-hook 'magit-filenotify-mode))

(use-package recentf
  :config
  (add-to-list 'recentf-exclude "\\elpa")
  (add-to-list 'recentf-exclude "private/tmp")
  (add-to-list 'recentf-exclude "\\target")
  (add-to-list 'recentf-exclude "\\build")
  (add-to-list 'recentf-exclude "\\bin")
  (recentf-mode))

(recentf-mode +1)

(use-package project
  :config
  (setq project-vc-extra-root-markers '(".git" ".project")
	project-list-file "~/.emacs.d/projects")
  (setq project-switch-commands
	'((project-find-file "Find file")
	  (project-find-regexp "Find regexp")
	  (project-find-dir "Find directory")
	  (project-dired "Dired")
	  (project-switch-to-buffer "Find Buffer")
	  (project-async-shell-command "Command")
	  (magit-project-status "Magit")))
  :bind
  (("C-c f" . project-find-file)
   ("C-c b" . project-switch-to-buffer)
   ("C-c r" . project-query-replace-regexp)
   ("C-c RET" . project-shell)
   ("C-c c" . project-compile)
   ("C-c !" . project-async-shell-command)
   ("C-c k" . project-kill-buffers)))

(global-set-key (kbd "C-x p") 'project-switch-project)


(el-get-bundle Wilfred/deadgrep :name deadgrep)
(use-package deadgrep
   :bind (("C-c g" . #'deadgrep)))

(add-hook 'deadgrep-mode-hook #'highline-mode-on)

(global-set-key (kbd "C-c g") 'deadgrep)

;; crypto
(setq auth-source-debug t
      auth-source-do-cache nil
      auth-sources '((:source "~/.authinfo.gpg")))

(setq starttls-use-gnutls t
      starttls-gnutls-program "gnutls-cli"
      starttls-extra-arguments '("--starttls" "--insecure"))

(use-package epa-file
  :init
  (epa-file-enable)
  :config
  (setq epg-gpg-program "/usr/bin/gpg2"
	epa-pinentry-mode 'loopback))

(el-get-bundle walseb/espy :name espy)
(use-package espy)

;; shell
(add-to-list 'exec-path "/usr/local/bin")
(use-package shell
  :config
  (progn
    (setq explicit-shell-file-name "bash")))

(use-package comint
  :defer t
  :config
  (progn
    (setf comint-prompt-read-only t
	  comint-use-prompt-regexp nil
          comint-history-isearch nil)
    (add-hook 'shell-mode-hook
              (lambda ()
		(define-key shell-mode-map (kbd "C-l") 'comint-clear-buffer)
		(define-key shell-mode-map
		  (kbd "C-r")
		  'comint-history-isearch-backward-regexp)))))

(el-get-bundle bash-completion)
(use-package bash-completion
  :disabled t
  :init
  (bash-completion-setup))

;; org-mode

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (:map org-mode-map
	      ("C-c C-w" . org-refile)
	      ("C-c C-x C-o" . org-clock-out)
	      ("C-c B" . org-switchb)
	      ("M-o" . ace-link-org))
  :config
  (progn
    (setq org-tags-column 60
	  org-hide-emphasis-markers t
	  org-hide-leading-stars t
	  org-startup-indented t
	  org-replace-disputed-keys nil
	  org-support-shift-select 'always
	  org-use-speed-commands t
	  org-log-done 'time
	  org-html-doctype "html5"
	  org-html-indent t
	  org-M-RET-may-split-line '((item . nil))
	  org-return-follows-link t)
    (add-hook 'org-mode-hook 'turn-on-font-lock)
    (add-hook 'org-mode-hook 'org-indent-mode)
    (add-hook 'org-mode-hook 'auto-fill-mode)
    (add-hook 'before-save-hook 'org-align-all-tags nil t)
    (define-key org-mode-map [S-right] 'windmove-right)
    (define-key org-mode-map [S-left]  'windmove-left)))

(use-package ob
  :init
  (require 'ob)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (org . t)
     (clojure . t)
     (dot . t)))

  (add-hook 'org-babel-after-execute-hook
	    (lambda ()
	      (when org-inline-image-overlays
		(org-redisplay-inline-images)))))


(use-package org-src
  :config
    (setq org-src-fontify-natively t
	  org-src-tab-acts-natively t
	  org-src-window-setup 'current-window
	  org-src-preserve-indentation t
	  org-edit-src-content-indentation 0
	  org-inline-image-overlays t))

(use-package org-crypt
  :config
  (progn
    (org-crypt-use-before-save-magic)
    (setq org-tags-exclude-from-inheritence '("crypt")
	  org-crypt-key nil)))

(el-get-bundle jakebox/org-preview-html
  :name org-preview-html)
(use-package org-preview-html
  :bind (:map org-mode-map
	      ("C-c C-e" . org-preview-html/preview))
  :config
  (setq org-preview-html/htmlfilename
	(concat "/tmp/" (make-temp-name "-") ".html")))

(use-package org-agenda
  :config
  (bind-key "C-c a" 'org-agenda)
  (setq org-use-fast-todo-selection t
	org-refile-targets '((nil :level . 1)
			     (org-agenda-files :level . 1))
	org-agenda-window-setup '(current-window-configuration)
	org-agenda-include-diary nil
	org-agenda-start-on-weekday 0
	org-deadline-warning-days 14
	org-highest-priority ?A
	org-lowest-priority ?C
	org-default-priority ?A
	org-agenda-prefix-format (cons
                                  '(agenda . " %i %-16:c%?-12t% s")
                                  (remove-if (lambda (x)
                                               (eq (car x) 'agenda))
                                             org-agenda-prefix-format))
	org-todo-keywords '((sequence "TODO(t)"
				      "|"
				      "NOW(n)"
				      "DONE(d)")))
  (define-key org-agenda-mode-map [S-right] 'windmove-right)
  (define-key org-agenda-mode-map [S-left]  'windmove-left)
  (add-hook 'org-agenda-mode-hook 'highline-mode-on))

(require 'org-tempo)

;; web

(el-get-bundle hniksic/emacs-htmlize
  :name htmlize)
(use-package htmlize)

(use-package shrface
  :config
  (shrface-basic)
  (shrface-trial)
  (setq shr-inhibit-images t
	shr-width 72
	shr-use-fonts nil
	shr-use-colors nil
	shr-discard-aria-hidden t
	shr-cookie-policy nil
	shr-image-animate nil
	shr-color-visible-luminance-min 80))

(el-get-bundle ace-link)
(use-package eww
  :preface
  (defun eww-toggle-image ()
    (interactive)
    (eww-reload)
    (setq shr-inhibit-images (not shr-inhibit-images))
    (message "Image is now %s"
	     (if shr-inhibit-images "off" "on")))

  (defun eww-browse-url-at-point ()
    (interactive)
    (let ((browse-url-browser-function 'eww-browse-url))
      (browse-url-at-point)))

  :bind
  (:map eww-mode-map
	("H" . eww-list-histories)
	("I" . eww-toggle-image)
	("o" . eww-browse-with-external-browser)
	("l" . ace-link-eww))
  :config
  (require 'shrface)
  (setq eww-form-checkbox-symbol "[ ]"
	eww-form-checkbox-selected-symbol "[X]"
	eww-search-prefix  "https://lite.duckduckgo.com/lite/?q="
	eww-download-directory "~/data/downloads"
	eww-bookmarks-directory "~/data/bookmarks"
	eww-history-limit 150
	eww-header-line-format "%u"
	eww-browse-url-new-window-is-tab nil)
  (add-hook 'eww-mode 'ace-link-mode))


(defun eww-wikipedia (text)
  (interactive (list (read-string "Wiki for: ")))
  (eww (format "https://en.m.wikipedia.org/wiki/Special:Search?search=%s"
               (url-encode-url text))))

(defun buffer-exists (bufname)
  (not (eq nil (get-buffer bufname))))

(defun eww-or-search ()
  (interactive)
  (if (buffer-exists "*eww*")
      (switch-to-buffer "*eww*")
    (eww-search-words)))

(defun firefox ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (browse-url (concat "file://" filename))))

;; tramp

(use-package tramp
  :config
  (setq tramp-default-method "ssh"
	tramp-auto-save-directory "~/.emacs.d/var/tramp"
	password-cache-expiry 3600
	buffer-auto-save-file-name nil
	tramp-completion-reread-directory-timeout nil)
  (progn
    (add-to-list 'tramp-default-proxies-alist
		 '(nil "\\`root\\'" "/ssh:%h:"))
    (add-to-list 'tramp-default-proxies-alist
		 '((regexp-quote (system-name)) nil nil))))


;; help

(el-get-bundle kai2nenobu/guide-key
  :name guide-key)
(use-package guide-key
  :init (guide-key-mode 1)
  :config
  (progn
    (setq guide-key/idle-delay 1)
    (setq guide-key/recursive-key-sequence-flag t)
    (setq guide-key/popup-window-position 'bottom)
    (setq guide-key/guide-key-sequence
          `("C-c" "C-x" "C-h"))))

(defalias 'yes-or-no-p 'y-or-n-p)
(load-file "~/.emacs.d/icylisper.el")

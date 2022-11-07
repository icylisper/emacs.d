(package-initialize)

(defconst init-file (expand-file-name "emacs-init.el" user-emacs-directory)
  "All configurations are stored in this file.")

(require 'cl)

(add-to-list 'load-path "~/lib/emacs/el-get/el-get")
(add-to-list 'load-path "~/lib/emacs/el-get")

(setq el-get-dir "~/lib/emacs/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))


(defun unfringe ()
  (interactive)
  (set-face-attribute 'fringe nil :background nil))

(add-to-list 'custom-theme-load-path "~/lib/emacs/themes/base16-emacs/build")
(set-face-attribute 'variable-pitch nil :font "inconsolata" :height 172)
(set-frame-font "hasklig 14" nil t)
;;(set-frame-font "monaco 12" nil t)

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
      resize-mini-windows nil
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

;; guide-key
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

(el-get-bundle jguenther/discover-my-major
  :name discover-my-major)
(use-package discover-my-major
  :config
  (bind-key "C-h m" 'discover-my-major))

;; windowing stuff
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

(advice-add 'load-theme
	    :around #'load-theme-advice)

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

(defun kill-matching-buffers-just-do-it ()
  "Kill buffers whose names match REGEXP, without asking."
  (interactive)
  (cl-letf (((symbol-function 'kill-buffer-ask) #'kill-buffer))
    (call-interactively #'kill-matching-buffers)))

(global-set-key "\C-xK" 'kill-matching-buffers-just-do-it)

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

;; expand region
(el-get-bundle expand-region)
(use-package expand-region
  :config
  (bind-key (kbd "C-=") 'er/expand-region))

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
  (setq completion-cycle-threshold 2
        completion-flex-nospace nil
        completion-pcm-complete-word-inserts-delimiters t
        completion-pcm-word-delimiters "-_./:| "
        completion-show-help t
        completions-format 'horizontal
        enable-recursive-minibuffers t
        read-answer-short t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        resize-mini-windows nil)
  (file-name-shadow-mode)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))

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

;; completion

(ido-mode -1)

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


;; ace-jump-mode

(el-get-bundle ace-jump-mode)
(use-package ace-jump-mode
  :init
  (autoload 'ace-jum-mode "ace-jump-mode" "Emacs quick move" t)
  (bind-key (kbd "C-c i") 'ace-jump-mode))


(use-package hippie-exp
  :init
  ;; force hippie-expand completions to be case-sensitive
  (defadvice hippie-expand (around hippie-expand-case-fold activate)
    "Try to do case-sensitive matching (not effective with all functions)."
    (let ((case-fold-search nil))
      ad-do-it))
  :config
  (setq hippie-expand-try-functions-list
	'(try-expand-dabbrev
	  try-expand-dabbrev-all-buffers
	  try-expand-dabbrev-from-kill
	  try-complete-file-name-partially
	  try-complete-file-name
	  try-expand-all-abbrevs
	  try-expand-list
	  try-expand-line
	  try-complete-lisp-symbol-partially
	  try-complete-lisp-symbol)))

(defun smart-tab ()
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
        (hippie-expand nil))
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (if (looking-at "\\_>")
         (hippie-expand nil)
        (indent-for-tab-command)))))
(global-set-key (kbd "TAB") 'smart-tab)


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

(el-get-bundle diasjorge/emacs-load-env-vars
  :name load-env-vars)

(defun load-if-exists (file)
  (when (file-exists-p file)
    (load-file file)))

(defun init! (mod)
  (let ((file (concat "~/.emacs.d/init.d/" (symbol-name mod) ".el")))
    (when (file-exists-p file)
      (load-file file))))

(init! 'lang-cl)
(init! 'lang-clojure)
(init! 'lang-elisp)
(init! 'lang-racket)
(init! 'lang-elixir)
(init! 'lang-rust)
(init! 'lang-ocaml)
(init! 'lang-rust)
(init! 'lang-dart)
(init! 'lang-python)
(init! 'lang-js)
(init! 'lang-go)
(init! 'sexp)
(init! 'format)
(init! 'crypto)
(init! 'org)
(init! 'project)
(init! 'sys-shell)
(init! 'net-http)
;;(init! 'net-mail)
(init! 'net-ssh)

;;(load-file "~/.emacs.d/icylisper.el")

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


;; completion
(ido-mode -1)

;; completion read

(setq completion-styles '(partial-completion substring)
      completion-ignore-case t
      read-buffer-completion-ignore-case t)

;; completions buffer

;; styles:  completion-styles-alist
;; category defaults: completion-category-defaults
;; catch-all completion-styles

(setq completion-show-help nil
      completions-format 'vertical
      completion-category-overrides '((file (styles basic substring))
				      (buffer (styles initials flex)
					      (cycle . 3))))

;;(el-get-bundle oantolin/icomplete-vertical :name icomplete-vertical)
(icomplete-vertical-mode 1)

;; custom-styles
;; (add-to-list 'completion-category-defaults '(cider (styles basic)))

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

(icomplete-vertical-mode 1)
;; (el-get-bundle oantolin/icomplete-vertical :name icomplete-vertical)

;; (add-to-list 'completion-category-defaults '(cider (styles basic)))

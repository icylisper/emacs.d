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

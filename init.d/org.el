;; org-mode
(add-to-list 'load-path "~/lib/emacs/el-get/org-mode/lisp")
(add-to-list 'load-path "~/lib/emacs/el-get/org-mode/contrib/lisp")

(el-get-bundle org-mode)
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
     (dot . t)
     (blockdiag . t)))

  (add-hook 'org-babel-after-execute-hook
	    (lambda ()
	      (when org-inline-image-overlays
		(org-redisplay-inline-images)))))

(el-get-bundle corpix/ob-blockdiag.el :name ob-blockdiag)

(use-package ob-blockdiag)


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

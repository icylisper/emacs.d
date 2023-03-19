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

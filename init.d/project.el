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

(el-get-bundle projectile)
(el-get-bundle find-file-in-project)
(defun ffip-create-pattern-file-finder (&rest patterns)
  (lexical-let ((patterns patterns))
    (lambda ()
      (interactive)
      (let ((ffip-patterns patterns))
        (find-file-in-project)))))
(use-package find-file-in-project
  :config
  (bind-key "C-c f" (ffip-create-pattern-file-finder
		     "*.clj" "*.cljs" "*.org"
		     "*.el" ".ml"
		     "*.rs"
		     ".janet"
		     ".graphql")))

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

(use-package projectile
  :init
  (projectile-global-mode)
  :defer (projectile-cleanup-known-projects)
  :diminish projectile-mode
  :config
  (setq projectile-switch-project-action 'projectile-dired
	projectile-find-dir-includes-top-level t
	projectile-track-known-projects-automatically nil
	projectile-remember-window-configs nil
	projectile-enable-caching nil
	projectile-indexing-method 'alien
	projectile-completion-system 'default
	projectile-require-project-root t
	projectile-mode-line '(:eval (format " (%s)"
					     (projectile-project-name)))
	projectile-sort-order 'recently-active)

  :bind (("C-c D" . projectile-dired)
	 ("C-c !" . projectile-run-command-in-root)
	 ("C-c b" . projectile-switch-to-buffer)))

(el-get-bundle wgrep)
(el-get-bundle dajva/rg.el :name rg)
(use-package rg
  :init
  (require 'rg)
  (rg-enable-default-bindings))

(rg-define-search rg-current-dir
  "search current dir"
  :query ask
  :format literal
  :dir project
  :confirm never
  :files "*.*"
  :flags ("--word-regexp")
  :menu ("Custom" "c" "Current"))

(add-hook 'rg-mode-hook #'highline-mode-on)



(defun lookup-doc ()
  (interactive))

;; transient
(transient-define-prefix projectile-transient-menu ()
  "A menu for projectile"
  [["File"
    ("f" "file" projectile-find-file)
    ("d" "dired" projectile-dired)
    ("D" "directory" projectile-find-dir)
    ("e" "recent" projectile-recentf)
    ("l" "file in dir" projectile-find-file-in-directory)
    ("G" "dwim" projectile-find-file-dwim)]

   ["Buffer"
    ("e" "recent" projectile-recentf)
    ("b" "buffer" projectile-switch-to-buffer)]

   ["Shell"
    ("/" "shell" projectile-run-shell-command-in-root)
    ("!" "shell" projectile-run-shell)]

   ["Search"
    ("o" "multi" projectile-multi-occur)
    ("s" "rg" rg-current-dir)
    ("r" "rgrep" rgrep)
    ("%" "replace" projectile-replace)]

   ["Build"
    ("c" "compile" projectile-compile-project)]

   ["Git"
    ("L" "Log" magit-log-head)]])

;; C-c prefix

(global-set-key (kbd "C-c p") 'projectile-transient-menu)
(global-set-key (kbd "C-c g") 'rg-current-dir)

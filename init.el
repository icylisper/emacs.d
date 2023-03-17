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

(el-get-bundle diasjorge/emacs-load-env-vars
  :name load-env-vars)

(el-get-bundle exec-path-from-shell)
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(defun load-if-exists (file)
  (when (file-exists-p file)
    (load-file file)))

(defun init! (mod)
  (let ((file (concat "~/.emacs.d/init.d/" (symbol-name mod) ".el")))
    (when (file-exists-p file)
      (load-file file))))

(init! 'theme)
(init! 'window)
(init! 'buffer)
(init! 'dired)
(init! 'lang)
(init! 'crypto)
(init! 'org)
(init! 'project)
(init! 'shell)
(init! 'web)
(init! 'help)
;;(init! 'mail)

(load-file "~/.emacs.d/icylisper.el")

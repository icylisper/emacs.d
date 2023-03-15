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

(set-face-attribute 'variable-pitch nil :font "inconsolata" :height 148)
(set-frame-font "hasklig 13" nil t)
;;(set-frame-font "inconsolata 13" nil t)


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
(el-get-bundle transient)
(package-initialize)


(use-package transient
     :config
     (setq transient-hide-during-minibuffer-read t
	   transient-show-popup 0.1))

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

(el-get-bundle diasjorge/emacs-load-env-vars
  :name load-env-vars)

(defun load-if-exists (file)
  (when (file-exists-p file)
    (load-file file)))

(defun init! (mod)
  (let ((file (concat "~/.emacs.d/init.d/" (symbol-name mod) ".el")))
    (when (file-exists-p file)
      (load-file file))))

(init! 'buffer)
(init! 'dired)
(init! 'lang)
(init! 'crypto)
(init! 'org)
(init! 'project)
(init! 'sys)
(init! 'web)
;(init! 'mail)

(load-file "~/.emacs.d/icylisper.el")

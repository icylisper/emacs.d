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

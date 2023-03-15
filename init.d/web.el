(el-get-bundle hniksic/emacs-htmlize
  :name htmlize)
(use-package htmlize)

(use-package shrface
  :config
  (shrface-basic)
  (shrface-trial)
  (setq shr-inhibit-images t
	shr-width 72
	shr-use-fonts nil
	shr-use-colors nil
	shr-discard-aria-hidden t
	shr-cookie-policy nil
	shr-image-animate nil
	shr-color-visible-luminance-min 80))

(el-get-bundle ace-link)
(use-package eww
  :preface
  (defun eww-toggle-image ()
    (interactive)
    (eww-reload)
    (setq shr-inhibit-images (not shr-inhibit-images))
    (message "Image is now %s"
	     (if shr-inhibit-images "off" "on")))

  (defun eww-browse-url-at-point ()
    (interactive)
    (let ((browse-url-browser-function 'eww-browse-url))
      (browse-url-at-point)))

  :bind
  (:map eww-mode-map
	("H" . eww-list-histories)
	("I" . eww-toggle-image)
	("o" . eww-browse-with-external-browser)
	("l" . ace-link-eww))
  :config
  (require 'shrface)
  (setq eww-form-checkbox-symbol "[ ]"
	eww-form-checkbox-selected-symbol "[X]"
	eww-search-prefix  "https://lite.duckduckgo.com/lite/?q="
	eww-download-directory "~/data/downloads"
	eww-bookmarks-directory "~/data/bookmarks"
	eww-history-limit 150
	eww-header-line-format "%u"
	eww-browse-url-new-window-is-tab nil)
  (add-hook 'eww-mode 'ace-link-mode))


(defun eww-wikipedia (text)
  (interactive (list (read-string "Wiki for: ")))
  (eww (format "https://en.m.wikipedia.org/wiki/Special:Search?search=%s"
               (url-encode-url text))))

(defun buffer-exists (bufname)
  (not (eq nil (get-buffer bufname))))

(defun eww-or-search ()
  (interactive)
  (if (buffer-exists "*eww*")
      (switch-to-buffer "*eww*")
    (eww-search-words)))

(defun firefox ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (browse-url (concat "file://" filename))))

(add-to-list 'load-path "~/lib/emacs/el-get/el-get/elfeed")

(el-get-bundle elfeed)
(el-get-bundle remyhonig/elfeed-org
  :name elfeed-org)
(defvar elfeed-show-switch-function #'switch-to-buffer)

(defun elfeed-show-find-window ()
  (cl-loop for window in (window-list)
           for buffer = (window-buffer window)
           for mode = (with-current-buffer buffer major-mode)
           when (eq mode 'elfeed-show-mode)
           return window))

(defun elfeed-show-in-other-buffer (buffer)
  (let ((target (elfeed-show-find-window)))
    (if target
        (setf (window-buffer target) buffer)
      (pop-to-buffer buffer))))

(defun switch-to-elfeed-entry-buffer ()
  (interactive)
  (let ((entry "*elfeed-entry*"))
    (if (eq (current-buffer) (get-buffer entry))
	(switch-to-buffer "*elfeed-search*")
      (if (get-buffer entry)
	  (switch-to-buffer "*elfeed-entry*")
	(elfeed)))))

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

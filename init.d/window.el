(setq window-combination-resize t
      resize-mini-windows nil
      max-mini-window-height 1)


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

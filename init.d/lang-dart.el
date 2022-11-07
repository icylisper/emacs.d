(el-get-bundle bradyt/dart-mode :name dart-mode)
(use-package dart-mode)

(el-get-bundle amake/flutter.el :name flutter)
(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload)))

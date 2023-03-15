(el-get-bundle gromnitsky/wordnut :wordnut)
(use-package wordnut)

(use-package abbrev
  :defer 5
  :diminish
  :hook
  ((text-mode prog-mode erc-mode LaTeX-mode) . abbrev-mode)
  (expand-load
   . (lambda ()
       (add-hook 'expand-expand-hook #'indent-according-to-mode)
       (add-hook 'expand-jump-hook #'indent-according-to-mode)))
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

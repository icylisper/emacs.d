(el-get-bundle pdf-tools)
(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (bind-keys :map
	     pdf-view-mode-map
             ("<s-spc>" .  pdf-view-scroll-down-or-previous-page)
             ("g"  . pdf-view-first-page)
             ("G"  . pdf-view-last-page)
             ("l"  . image-forward-hscroll)
             ("h"  . image-backward-hscroll)
             ("j"  . pdf-view-next-page)
             ("k"  . pdf-view-previous-page)
             ("e"  . pdf-view-goto-page)
             ("u"  . pdf-view-revert-buffer)
             ("al" . pdf-annot-list-annotations)
             ("ad" . pdf-annot-delete)
             ("aa" . pdf-annot-attachment-dired)
             ("am" . pdf-annot-add-markup-annotation)
             ("at" . pdf-annot-add-text-annotation)
             ("y"  . pdf-view-kill-ring-save)
             ("i"  . pdf-misc-display-metadata)
             ("s"  . pdf-occur)
             ("b"  . pdf-view-set-slice-from-bounding-box)
             ("r"  . pdf-view-reset-slice)))

(use-package pdf-view
  :config
  (add-hook 'pdf-view-mode-hook #'pdf-view-fit-page-to-window)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-resize-factor 1.10))

(el-get-bundle jeremy-compostella/pdfgrep
  :name pdfgrep)
(use-package pdf-grep
  :init
  (require 'pdfgrep)
  (pdfgrep-mode))

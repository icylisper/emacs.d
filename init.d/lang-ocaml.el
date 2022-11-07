
(el-get-bundle tuareg-mode)
(use-package tuareg-mode
  :mode ("\\.ml[ily]?$" . tuareg-mode)
  :config
  (progn
    (dolist (var (car (read-from-string
		       (shell-command-to-string "opam config env --sexp"))))
      (setenv (car var) (cadr var)))
    (setq exec-path (split-string (getenv "PATH") path-separator))
    (dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi"))
      (add-to-list 'completion-ignored-extensions ext))
    (add-to-list 'auto-mode-alist '("dune$" . tuareg-jbuild-mode))
    (setq tuareg-indent-align-with-first-arg t
	  tuareg-match-patterns-aligned t)
    (add-hook 'tuareg-mode-hook
    	      (lambda ()
		(setq compile-command "dune build @install"
		      indent-line-function 'ocp-indent-line)))))

(el-get-bundle utop
  :url "https://raw.githubusercontent.com/ocaml-community/utop/master/src/top/utop.el")
(use-package utop
  :init
  (dolist (var (car (read-from-string
		     (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var)))
  (setq exec-path (split-string (getenv "PATH") path-separator))

  (autoload 'utop "utop" "Toplevel for OCaml" t)
  (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
  :config
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  (setq utop-command "opam config exec -- utop -emacs"))

(el-get-bundle dune
  :url "https://github.com/ocaml/dune/blob/main/editor-integration/emacs/dune.el")

(use-package dune)

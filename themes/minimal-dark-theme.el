(deftheme minimal-dark
  "A minimal theme based on xterm-256 color set [dark version]")

(let ((black016 "#000000")   ;; black
      (white231 "#ffffff")   ;; white
      (gray233  "#121212")   ;; background
      (gray243  "#919191")   ;; text, code
      (gray239  "#4e4e4e")   ;; comments
      (gray235  "#262626")   ;; line numbers, current line
      (red160   "#d70000")   ;; match
      (blue066  "#5f8787")
      (green066  "#55997a")
      (yellow066  "#87875f"))  ;; strings

  ;; Set faces
  (custom-theme-set-faces
   'minimal-dark

   `(default ((t (:foreground ,gray243 :background ,gray233))))
   `(cursor  ((t (:background ,white231))))

   ;; Highlighting faces
   `(fringe    ((t (:background ,black016))))
   `(highlight ((t (:background ,black016))))
   `(region    ((t (:background ,gray235))))

   ;; Font lock faces
   `(font-lock-string-face        ((t (:foreground ,gray239))))
   `(font-lock-comment-face       ((t (:foreground ,gray239))))
   `(font-lock-constant-face      ((t (:foreground ,gray243))))
   `(font-lock-function-name-face ((t (:foreground ,yellow066))))
   `(font-lock-variable-name-face ((t (:foreground ,gray243))))
   `(font-lock-builtin-face       ((t (:foreground ,gray243 :weight bold))))
   `(font-lock-keyword-face       ((t (:foreground ,green066))))
   `(font-lock-type-face          ((t (:foreground ,blue066))))

   ;;parens
   `(show-paren-mismatch   ((t (:foreground ,blue066 :background ,black016 :weight bold))))
   `(show-paren-match      ((t (:foreground ,red160  :background ,black016 :weight bold))))

   ;; line numbers, current line, mode-line
   ;`(hl-line-face ((t (:background ,gray235 :weight bold))))
   `(hl-line      ((t (:background ,gray235))))
   `(linum        ((t (:background ,gray235))))
   `(mode-line    ((t (:foreground ,gray233 :background ,gray243 :box nil))))

   `(rainbow-delimiters-depth-1-face                  ((t (:foreground "gray35"))))
   `(rainbow-delimiters-depth-2-face                  ((t (:foreground "gray35"))))
   `(rainbow-delimiters-depth-3-face                  ((t (:foreground "gray35"))))
   `(rainbow-delimiters-depth-4-face                  ((t (:foreground "gray35"))))
   `(rainbow-delimiters-depth-5-face                  ((t (:foreground "gray35"))))
   `(rainbow-delimiters-depth-6-face                  ((t (:foreground "gray35"))))
   `(rainbow-delimiters-depth-7-face                  ((t (:foreground "gray35"))))
   `(rainbow-delimiters-depth-8-face                  ((t (:foreground "gray35"))))
   `(rainbow-delimiters-depth-9-face                  ((t (:foreground "gray35"))))
   `(rainbow-delimiters-depth-10-face                 ((t (:foreground "gray35"))))
   `(rainbow-delimiters-depth-11-face                 ((t (:foreground "gray35"))))
   `(rainbow-delimiters-depth-12-face                 ((t (:foreground "gray35"))))

   `(minibuffer-prompt ((t (:foreground "#55997a"))))
   ;; web-mode
   `(web-mode-html-tag-face          ((t (:foreground ,gray243 :weight bold))))
   `(web-mode-html-attr-name-face    ((t (:foreground ,gray243 ))))
   `(web-mode-css-property-name-face ((t (:foreground ,gray243 :weight bold))))
   `(web-mode-keyword-face           ((t (:foreground ,gray243 :weight bold))))
   `(web-mode-builtin-face           ((t (:foreground ,gray243 :weight bold)))))

  ;; Set variables
  (custom-theme-set-variables
   'minimal-dark
   ;'(other variables)
   )
)

(provide-theme 'minimal-dark)


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; minimal-dark-theme.el ends here

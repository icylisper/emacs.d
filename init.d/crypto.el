(setq auth-source-debug t
      auth-source-do-cache nil
      auth-sources '((:source "~/.authinfo.gpg")))

(setq starttls-use-gnutls t
      starttls-gnutls-program "gnutls-cli"
      starttls-extra-arguments '("--starttls" "--insecure"))

(use-package epa-file
  :init
  (epa-file-enable)
  :config
  (setq epg-gpg-program "/usr/bin/gpg2"
	epa-pinentry-mode 'loopback))

(el-get-bundle walseb/espy :name espy)
(use-package espy)

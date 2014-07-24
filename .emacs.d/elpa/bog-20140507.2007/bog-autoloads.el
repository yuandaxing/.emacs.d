;;; bog-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (bog-mode bog-commander bog-search-citekey-on-web
;;;;;;  bog-create-combined-bib bog-clean-and-rename-staged-bibs
;;;;;;  bog-find-citekey-bib bog-rename-staged-file-to-citekey bog-find-citekey-file)
;;;;;;  "bog" "bog.el" (21366 18306 644245 600000))
;;; Generated autoloads from bog.el

(autoload 'bog-find-citekey-file "bog" "\
Open citekey-associated file.

The citekey will be taken from the text under point if it matches
`bog-citekey-format' or from the current subtree using
`bog-citekey-func'.

With prefix argument NO-CONTEXT, a prompt will open to select
from citekeys for all associated files. This same prompt will be
opened if locating a citekey from context fails.

\(fn &optional NO-CONTEXT)" t nil)

(autoload 'bog-rename-staged-file-to-citekey "bog" "\
Rename citekey file in `bog-stage-directory' with `bog-file-renaming-func'.

The citekey will be taken from the text under point if it matches
`bog-citekey-format' or from the current subtree using
`bog-citekey-func'.

With prefix argument NO-CONTEXT, a prompt will open to select
from citekeys for all associated files. This same prompt will be
opened if locating a citekey from context fails.

\(fn &optional NO-CONTEXT)" t nil)

(autoload 'bog-find-citekey-bib "bog" "\
Open BibTeX file for a citekey.

The citekey will be taken from the text under point if it matches
`bog-citekey-format' or from the current subtree using
`bog-citekey-func'.

With prefix argument NO-CONTEXT, a prompt will open to select
from citekeys for all BibTeX files. This same prompt will be
opened if locating a citekey from context fails.

\(fn &optional NO-CONTEXT)" t nil)

(autoload 'bog-clean-and-rename-staged-bibs "bog" "\
Clean and rename BibTeX files in `bog-stage-directory'.

New BibTeX files are searched for in `bog-stage-directory', and
`bog-prepare-bib-file' will be run one each file before it is
moved to `bog-bib-directory'/<citekey>.bib.

This function is only useful if you use the non-standard setup of
one entry per BibTeX file.

\(fn)" t nil)

(autoload 'bog-create-combined-bib "bog" "\
Create buffer that has entries for all citekeys in buffer.

\(fn)" t nil)

(autoload 'bog-search-citekey-on-web "bog" "\
Open browser and perform query based for a citekey.

The URL will be taken from `bog-web-search-url'.

The citekey is split by groups in `bog-citekey-format' and joined by
\"+\" to form the query string.

The citekey will be taken from the text under point if it matches
`bog-citekey-format' or from the current subtree using
`bog-citekey-func'.

With prefix argument NO-CONTEXT, a prompt will open to select
from all citekeys present in notes. This same prompt will be
opened if locating a citekey from context fails.

\(fn &optional NO-CONTEXT)" t nil)

(autoload 'bog-commander "bog" "\
Execute a Bog command with a single letter.

The user is prompted for a single character indicating the action
to invoke. Press \"?\" to describe available actions.

See `def-bog-commander-method' for defining new methods.

\(fn)" t nil)

(autoload 'bog-mode "bog" "\
Toggle Bog in this buffer.
With a prefix argument ARG, enable `bog-mode' if ARG is positive,
and disable it otherwise. If called from Lisp, enable the mode if
ARG is omitted or nil.

\\{bog-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("bog-pkg.el") (21366 18306 781959 300000))

;;;***

(provide 'bog-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bog-autoloads.el ends here

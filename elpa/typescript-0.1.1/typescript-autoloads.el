;;; typescript-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (typescript-mode) "typescript" "typescript.el"
;;;;;;  (22248 2489 267751 545000))
;;; Generated autoloads from typescript.el

(autoload 'typescript-mode "typescript" "\
Major mode for editing typescript.

Key bindings:

\\{typescript-mode-map}

\(fn)" t nil)

(eval-after-load 'folding '(when (fboundp 'folding-add-to-marks-list) (folding-add-to-marks-list 'typescript-mode "// {{{" "// }}}")))

(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))

;;;***

;;;### (autoloads nil nil ("typescript-pkg.el") (22248 2489 391075
;;;;;;  871000))

;;;***

(provide 'typescript-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; typescript-autoloads.el ends here

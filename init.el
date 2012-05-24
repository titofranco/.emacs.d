;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Load path etc.

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Load up ELPA, the package manager

(add-to-list 'load-path dotfiles-dir)

(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))

(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)
(require 'starter-kit-elpa)

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; backport some functionality to Emacs 22 if needed
(require 'dominating-file)

;; Load up starter kit customizations

(require 'starter-kit-defuns)
(require 'starter-kit-bindings)
(require 'starter-kit-misc)
(require 'starter-kit-registers)
(require 'starter-kit-eshell)
(require 'starter-kit-lisp)
(require 'starter-kit-perl)
(require 'starter-kit-ruby)
(require 'starter-kit-js)

(regen-autoloads)
(load custom-file 'noerror)

;; You can keep system- or user-specific customizations here
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))
(if (file-exists-p user-specific-config) (load user-specific-config))

;;; init.el ends here

;;CUSTOM CONFIG

(display-time)
(setq line-number-mode t)
(global-linum-mode 1)
(line-number-mode t)
(column-number-mode 1)
(setq scroll-bar-mode nil)

(setq user-mail-address "titofranco@gmail.com")
(setq user-full-name "Carlos Guisao")

;; add all the directories in .emacs.d/vendor/ to the path
(let* ((files (directory-files "~/.emacs.d/vendor" t "[^\.+]")))
 (mapcar (lambda (d) (add-to-list 'load-path d)) files))

;; Twilight mode
(load-file "~/.emacs.d/vendor/twilight/twilight.el")
(color-theme-initialize)
(color-theme-twilight)

;; js2-mode from https://github.com/mooz/js2-mode
(setq load-path (append (list (expand-file-name "~/.emacs.d/js2-mode")) load-path))
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;; rhtml-mode
(add-to-list 'load-path "~/.emacs.d/vendor/rhtml")
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
     	  (lambda () (rinari-launch)))

;; MuMaMo-Mode for rhtml files
;;(add-to-list 'load-path "~/.emacs.d/vendor/nxhtml/util")
;;(require 'mumamo-fun)
;;(setq mumamo-chunk-coloring 'submode-colored)
;;(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . eruby-html-mumamo))
;;(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-html-mumamo))

(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.css\\.erb\\'" . css-mode))

;; nice parentheses
(setq-default show-trailing-whitespace t)
(show-paren-mode t)
(setq show-paren-style 'expression)

(set-face-attribute 'default (selected-frame) :height 120)

;; Allow maximize window
(load-file "~/.emacs.d/vendor/maxframe.el")
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

;allow copy-paste system
(setq x-select-enable-clipboard t)

;; don't use tabs
(setq-default indent-tabs-mode nil)

;; Remove trailing whitespace after is saved
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; remove scrollbar
(scroll-bar-mode -1)

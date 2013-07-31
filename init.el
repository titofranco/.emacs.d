;; add all the directories in .emacs.d/vendor/ to the path
(let* ((files (directory-files "~/.emacs.d/vendor" t "[^\.+]")))
 (mapcar (lambda (d) (add-to-list 'load-path d)) files))

;;Starter Kit
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;MY CONFIG

(display-time)
(setq line-number-mode t)
(global-linum-mode 1)
(line-number-mode t)
(column-number-mode 1)
(setq scroll-bar-mode nil)

(setq user-mail-address "titofranco@gmail.com")
(setq user-full-name "Carlos Guisao")

;; Twilight mode
(require 'color-theme)
(load-file "~/.emacs.d/vendor/twilight/twilight.el")
(color-theme-twilight)

;; js2-mode from https://github.com/mooz/js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\.erb$" . rhtml-mode))

;;; rhtml mode
(add-to-list 'load-path "~/.emacs.d/vendor/rhtml")
(require 'rhtml-mode)

(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.css\\.less\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.css\\.scss\\'" . css-mode))
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

;; Font family and size
(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 140)

;;Smooth Scrolling
(load-file "~/.emacs.d/vendor/smooth-scrolling.el")
(require 'smooth-scrolling)

;; MuMaMo-Mode for rhtml files
;;(add-to-list 'load-path "~/.emacs.d/vendor/nxhtml/util")
;;(require 'mumamo-fun)
;;(setq mumamo-chunk-coloring 'submode-colored)
;;(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . eruby-html-mumamo))
;;(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-html-mumamo))

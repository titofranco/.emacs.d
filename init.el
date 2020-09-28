;;Starter Kit
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;;MY CONFIG
(require 'cl)
(require 'ido) ;;improves the speed at which you open files and buffers
(require 'ffap) ;;replaces certain key bindings for finding files
(require 'uniquify) ;; give the buffers distinct names
(require 'ansi-color)
(require 'recentf) ;; builds a list of recently opened files
(require 'dired-x)
(require 'compile)

(ido-mode t)
(ido-ubiquitous-mode t)
(display-time)
(setq line-number-mode t)
(global-linum-mode 1)
(line-number-mode t)
(column-number-mode 1)
(setq scroll-bar-mode nil)
(menu-bar-mode -1)
(normal-erase-is-backspace-mode 1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq inhibit-startup-message t)
(setq save-abbrevs nil)
(setq suggest-key-bindings t)
(setq vc-follow-symlinks t)

(setq user-mail-address "titofranco@gmail.com")
(setq user-full-name "Carlos Guisao")

;; add all the directories in .emacs.d/vendor/ to the path
(let* ((files (directory-files "~/.emacs.d/vendor" t "[^\.+]")))
 (mapcar (lambda (d) (add-to-list 'load-path d)) files))

;;; rhtml mode
(add-to-list 'load-path "~/.emacs.d/vendor/rhtml")
(require 'rhtml-mode)

;; Twilight mode
(require 'color-theme)
(load-file "~/.emacs.d/vendor/twilight/twilight.el")
(color-theme-twilight)

;; rake
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))

;; js2-mode from https://github.com/mooz/js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\.erb$" . rhtml-mode))
;; JS indentation
 (setq js-indent-level 2)
(setq js2-basic-offset 2)

(setq css-indent-offset 2)

;;Coffee mode
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(setq coffee-tab-width 2)

(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . less-css-mode))

(add-to-list 'auto-mode-alist '("\\.css\\.erb\\'" . css-mode))



;;handlebars mode
(load-file "~/.emacs.d/vendor/handlebars-mode.el")
(require 'handlebars-mode)

;; nice parentheses
(setq-default show-trailing-whitespace t)
(show-paren-mode t)
(setq show-paren-style 'expression)

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
(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 180)

;;yasnippet mode
(yas-global-mode 1)

;; Feature mode - Cucumber
(add-to-list 'load-path "~/.emacs.d/elpa/feature-mode-0.4")

;; Rinari
;; (add-to-list 'load-path "~/.emacs.d/elpa/rinari")
;; (require 'rinari)

;; Keybinds
(global-set-key (kbd "M-g") 'goto-line)

;; Disable automatic line break
(add-hook 'html-mode-hook 'turn-off-auto-fill)
(add-hook 'js2-mode-hook 'turn-off-auto-fill)
(add-hook 'ruby-mode-hook 'turn-off-auto-fill)
(auto-fill-mode -1)
(turn-off-auto-fill)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-off-auto-fill)

;; MuMaMo-Mode for rhtml files
;;(add-to-list 'load-path "~/.emacs.d/vendor/nxhtml/util")
;;(require 'mumamo-fun)
;;(setq mumamo-chunk-coloring 'submode-colored)
;;(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . eruby-html-mumamo))
;;(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-html-mumamo))

;;autocomplete
(add-to-list 'load-path "~/.emacs.d/vendor/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete/ac-dict")
(ac-config-default)

;; Dont create backup #.filemame
(setq create-lockfiles nil)

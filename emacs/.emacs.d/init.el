;; PACKAGE SETTINGS ------------------------------------------------------------
(require 'package)

(add-to-list 'package-archives
  '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
  '("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-initialize)

;; INSTALL PACKAGE MANAGER -----------------------------------------------------
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; ADD LOAD DIRECTORIES --------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/q-mode") ; Installed before config

;; LOAD CONFIGS ----------------------------------------------------------------
(require 'radek-functions)
(require 'radek-defaults)
(require 'radek-gui)
(require 'radek-languages)
(require 'radek-utils)
(require 'radek-keybindings)

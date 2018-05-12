(package-initialize)
(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives 
  '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives 
  '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives 
  '("melpa-stable" . "http://stable.melpa.org/packages/"))

;; INSTALL PACKAGE MANAGER
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; ENSURE ALL PACKAGES
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package evil
  :ensure t
  :config

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode))

  (evil-mode t)

  (use-package evil-indent-textobject
    :ensure t))

(use-package key-chord
  :ensure t
  :config
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-mode 1))

(use-package helm
  :ensure t
  :diminish helm-mode
  :commands helm-mode
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40))

(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package elpy
  :ensure t
  :config
  (elpy-enable))

(use-package haskell-mode
  :ensure t)

; (use-package challenger-deep-theme
;   :ensure t
;   :config
;   (load-theme 'challenger-deep t))

(use-package atom-one-dark-theme
  :ensure t
  :config
  (load-theme 'atom-one-dark t))


;; SET A NICER FONT
(add-to-list 'default-frame-alist '(font . "Fira Code-13"))
(set-face-attribute 'default t :font "Fira Code-13")
(setq-default line-spacing 4)

;; SETTINGS
(cd "/Users/radek/Library/Mobile Documents/com~apple~CloudDocs/Radek HD/Work/PROJECTS")
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(column-number-mode t)
(setq tab-width 4)
(global-hl-line-mode 1)
(global-linum-mode t)

;; Since a lot of special characters are written using Alt (Option) key,
;; let's make Fn key a meta key instead
(setq mac-function-modifier 'meta)
(setq mac-option-modifier nil)


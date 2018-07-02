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

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)

  (use-package counsel
	:ensure t
	:config
	(counsel-mode 1)))

(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package cider
  :ensure t)

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-g") (lambda () (interactive) (magit-status))))

(use-package haskell-mode
  :ensure t)

(use-package evil
  :ensure t
  :config

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode))

  (evil-mode t)

  (use-package evil-indent-textobject
    :ensure t)

  (use-package evil-commentary
    :ensure t
    :config
    (evil-commentary-mode))

  (use-package evil-magit
    :ensure t
    :config
    (setq evil-magit-state 'normal)))

(defun open-terminal ()
  (split-window-horizontally)
  (next-multiframe-window)
  (eshell))

;; `advanced-next-buffer` fun taken from https://stackoverflow.com/a/14511461/7699136
(setq skippable-buffers '("*Messages*" "*scratch*" "*Help*" "*Buffer List*" "*eshell*" "*ansi-term*" "*Compile-Log*"))
(defun advanced-next-buffer ()
  "next-buffer that skips certain buffers"
  (interactive)
  (next-buffer)
  (while (member (buffer-name) skippable-buffers)
    (next-buffer)))

(use-package key-chord
  :ensure t
  :config
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-normal-state-map "tr" (lambda () (interactive) (open-terminal)))
  (key-chord-define evil-normal-state-map "ls" (lambda () (interactive) (buffer-menu)))
  (key-chord-define evil-normal-state-map "gt" (lambda () (interactive) (advanced-next-buffer)))
  (key-chord-mode 1))

(use-package linum-relative
  :ensure t
  :config
  (setq linum-relative-format "  %3s  ")
  (linum-relative-global-mode))

(use-package monokai-theme
 :ensure t
 :config
 (load-theme 'monokai t))

;; SET A NICER FONT
(add-to-list 'default-frame-alist '(font . "Fira Code-14:weight=Light"))
(set-face-attribute 'default t :font "Fira Code-14")
(setq-default line-spacing 6)

;; SETTINGS
(cd "/Users/radek/Documents/Work/PROJECTS")
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq tab-width 4)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(column-number-mode t)
(global-hl-line-mode 1)
(desktop-save-mode 1)

;; Since a lot of special characters are written using Alt (Option) key,
;; let's make Fn key a meta key instead
(setq mac-function-modifier 'meta)
(setq mac-option-modifier nil)

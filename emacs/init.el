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

;; CUSTOM FUNCTIONS
(defun open-terminal ()
  (split-window-horizontally)
  (next-multiframe-window)
  (shrink-window-horizontally 100000)
  (enlarge-window-horizontally 70)
  (eshell))

(setq skippable-buffers '("*Messages*" "*scratch*" "*Help*" "*Buffer List*"
			  "*eshell*" "*ansi-term*" "*Compile-Log*"))

(defun advanced-switch-buffer (fn)
  "Buffer switch that skips certain buffers.
Can be used with both `next-buffer` and `previous-buffer`.

Inspired by https://stackoverflow.com/a/14511461/7699136"
  (interactive)
  (funcall fn)
  (while (member (buffer-name) skippable-buffers)
    (funcall fn)))

(defvar bw-directions-list
  '((?h windmove-left)
    (?j windmove-down)
    (?k windmove-up)
    (?l windmove-right))
  "List of mappings between the key and corresponding windmove action.")

(defun bw-func (char)
  "Return the correct windmove action based on provided CHAR from `bw-directions-list`."
  (nth 1 (assoc char bw-directions-list)))

(defun better-windmove (dir)
  "Vim-like window navigation using hjkl keys.

To completely mimic Vim behavior, bind `better-windmove` to C-w.
Note that `evil-mode` already does this, however `better-windmove`
can be still used throughout all Vim modes and on a different binding.

      (global-set-key (kbd \"C-w\") 'better-windmove)
  "
  (interactive "cDirection (h/j/k/l)?")
  (let ((action (bw-func dir)))
    (if (not (eq action nil))
        (funcall action)
        (user-error "Direction not recognised"))))

(defun confirm-kill-emacs (confirm)
  "Ask before quitting Emacs"
  (interactive "cQuit? (y/n) ")
  (if (eq confirm 121)
      (kill-emacs)))

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

(use-package indent-guide
  :ensure t
  :config
  (indent-guide-global-mode))

(use-package column-enforce-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'column-enforce-mode))

(use-package cider
  :ensure t)

(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1)
  (setq company-tooltip-idle-delay 0.1)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package magit
  :ensure t
  :config

  (use-package evil-magit
    :ensure t
    :config
    (setq evil-magit-state 'normal)))

(use-package haskell-mode
  :ensure t)

(use-package js2-mode
  :ensure t
  :config
  (setq js2-strict-missing-semi-warning nil)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode)))

(use-package dtrt-indent
  :ensure t
  :config
  (dtrt-indent-global-mode 1))

(use-package autopair
  :ensure t
  :config
  (autopair-global-mode 1))

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
    (evil-commentary-mode)))

(use-package neotree
  :ensure t
  :config

  (use-package all-the-icons
    :ensure t
    :config

    (use-package all-the-icons-ivy
      :ensure t)

    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))))

(use-package key-chord
  :ensure t
  :config
  (setq key-chord-two-keys-delay 0.3)
  (define-key evil-insert-state-map (kbd "C-w") 'better-windmove)
  (define-key evil-normal-state-map (kbd "C-q") 'confirm-kill-emacs)
  (define-key evil-normal-state-map (kbd "C-h") 'ns-do-hide-emacs)
  (define-key evil-normal-state-map (kbd "M-h") 'help-for-help)
  (define-key company-active-map (kbd "TAB") 'company-select-next-or-abort)
  (define-key company-active-map [tab] 'company-select-next-or-abort)
  (evil-define-key 'normal neotree-mode-map (kbd "o") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "r") 'neotree-refresh)
  (global-set-key (kbd "C-g") (lambda () (interactive) (magit-status)))
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "JK" 'evil-normal-state)
  (key-chord-define evil-normal-state-map "tr" (lambda () (interactive) (open-terminal)))
  (key-chord-define evil-normal-state-map "ls" (lambda () (interactive) (buffer-menu)))
  (key-chord-define evil-normal-state-map "gt" (lambda () (interactive) (advanced-switch-buffer 'next-buffer)))
  (key-chord-define evil-normal-state-map "gz" (lambda () (interactive) (advanced-switch-buffer 'previous-buffer)))
  (key-chord-mode 1))

(use-package linum-relative
  :ensure t
  :config
  (setq linum-relative-format "  %3s  ")
  (linum-relative-global-mode 1))

;; THEME AND POWERLINE
(use-package monokai-theme
 :ensure t
 :config
 (load-theme 'monokai 1))

(defface my-pl-gui-style
  '((t (:foreground "#5E5E59" :background "#2F3029" :box nil)))
  "My custom GUI powerline face")
(defface my-pl-active-gui-style
  '((t (:foreground "#BDB9B1" :background "#2F3029" :box nil)))
  "My custom GUI powerline face for the first segment")

(defface my-pl-term-style
  '((t (:foreground "#5E5E59" :background "#303030" :box nil)))
  "My custom terminal powerline face")
(defface my-pl-active-term-style
  '((t (:foreground "#BDB9B1" :background "#303030" :box nil)))
  "My custom terminal powerline face for the first segment")

(defun minimal-powerline-theme ()
  "Set up my custom Powerline with Evil indicators."
  (setq powerline-height 30)
  (setq powerline-display-hud nil)
  (setq powerline-default-separator (if (display-graphic-p) 'box nil))
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (gui (display-graphic-p))
                          (seg1
                            (if gui
                              (if active 'my-pl-active-gui-style 'my-pl-gui-style)
                              (if active 'my-pl-active-term-style 'my-pl-term-style)))
                          (seg2 (if gui 'my-pl-gui-style 'my-pl-term-style))
                          (seg3 (if gui 'my-pl-gui-style 'my-pl-term-style))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (let ((evil-face (powerline-evil-face)))
                                       (if evil-mode
                                           (powerline-raw (powerline-evil-tag) evil-face)
                                         ))
                                     (if evil-mode
                                         (funcall separator-left (powerline-evil-face) seg1))
                                     (powerline-buffer-id seg1 'l)
                                     (powerline-raw "[%*]" seg1 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format seg1 'l))
                                     (powerline-raw " " seg1)
                                     (funcall separator-left seg1 seg2)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object seg2 'l))
                                     (powerline-major-mode seg2 'l)
                                     (powerline-process seg2)
                                     (powerline-minor-modes seg2 'l)
                                     (powerline-narrow seg2 'l)
                                     (powerline-raw " " seg2)
                                     (funcall separator-left seg2 seg3)
                                     (when (bound-and-true-p nyan-mode)
                                       (powerline-raw (list (nyan-create)) seg3 'l))))
                          (rhs (list (powerline-raw global-mode-string seg3 'r)
                                     (powerline-vc seg3 'r)
                                     (funcall separator-right seg3 seg2)
                                     (unless window-system
                                       (powerline-raw (char-to-string #xe0a1) seg2 'l))
                                     (powerline-raw "%4l" seg2 'l)
                                     (powerline-raw ":" seg2 'l)
                                     (powerline-raw "%3c" seg2 'r)
                                     (funcall separator-right seg2 seg1)
                                     (powerline-raw " " seg1)
                                     (powerline-raw "%6p" seg1 'r)
                                     (when powerline-display-hud
                                       (powerline-hud seg1 seg3)))))
                     (concat (powerline-render lhs)
                             (powerline-fill seg3 (powerline-width rhs))
                             (powerline-render rhs)))))))

(use-package powerline
  :ensure t
  :config

  (use-package powerline-evil
    :ensure t)

  (minimal-powerline-theme))

;; SET A NICER FONT
(add-to-list 'default-frame-alist '(font . "Fira Code-13:weight=Light"))
(set-face-attribute 'default t :font "Fira Code-13")
(setq-default line-spacing 6)

;; GLOBAL SETTINGS
(cd "/Users/radek/Documents/Work/PROJECTS")
(setq inhibit-startup-message t
      ring-bell-function 'ignore
      make-backup-files nil
      scroll-step 1
      scroll-conservatively 10000)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(column-number-mode 1)
(global-hl-line-mode 1)
(desktop-save-mode 1)

;; Since a lot of special characters on a Czech keyboard layout
;; are written using Option key, let's keep it disabled for Emacs
;; bindings and set Meta to Fn and Control keys (both for better UX).
(setq mac-option-modifier nil)
(setq mac-function-modifier 'meta)
(setq mac-control-modifier 'meta)

;; Also set Control to Cmd key to avoid broken pinky
(setq mac-command-modifier 'control)

;; VIM EMULATION ---------------------------------------------------------------
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

  (use-package evil-commentary          ; Commenting
    :ensure t
    :config
    (evil-commentary-mode)))

(use-package linum-relative             ; Relative line numbers
  :ensure t
  :config
  (setq linum-relative-format "  %3s  ")
  (setq linum-relative-current-symbol "")
  (linum-relative-global-mode 1))

;; AUTOCOMPLETION --------------------------------------------------------------
(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0)
  (setq company-tooltip-idle-delay 0)
  (add-hook 'after-init-hook 'global-company-mode)
  (add-to-list 'company-backends 'merlin-company-backend)
  (add-hook 'eshell-mode-hook (lambda () (company-mode -1)) 'append))

;; CODE UTILS ------------------------------------------------------------------
(use-package dtrt-indent                ; Adapt indentation
  :ensure t
  :config
  (dtrt-indent-global-mode 1))

(use-package column-enforce-mode        ; Max line length
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'column-enforce-mode))

(use-package autopair                   ; Automatic braces
  :ensure t
  :config
  (autopair-global-mode 1))

(use-package highlight-numbers          ; Highlighting numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package rainbow-delimiters         ; Rainbow pair highlights for LISP
  :ensure t
  :config
  (add-hook 'find-file-hook
    (lambda ()
      (when (string= (file-name-extension buffer-file-name) "clj")
      (rainbow-delimiters-mode 1)))))

;; VERSION CONTROL (GIT) -------------------------------------------------------
(use-package magit
  :ensure t
  :config

  (use-package evil-magit
    :ensure t
    :config
    (setq evil-magit-state 'normal)))

;; PATH NAVIGATION -------------------------------------------------------------
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)

  (use-package counsel
    :ensure t
    :config
    (counsel-mode 1)))

(use-package neotree
  :ensure t
  :config

  (use-package all-the-icons
    :ensure t
    :config

    (use-package all-the-icons-ivy
      :ensure t)

    (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))
    (setq neo-window-width 32)
    (setq neo-show-hidden-files t))


(provide 'radek-utils)

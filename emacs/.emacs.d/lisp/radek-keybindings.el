(use-package key-chord
  :ensure t
  :config
  (setq key-chord-one-key-delay 0.2)
  (setq key-chord-two-keys-delay 0.1)

  (define-key                           ; Editor shortcuts
    evil-insert-state-map (kbd "C-w") 'better-windmove)
  (define-key
    evil-normal-state-map (kbd "C-q") 'confirm-kill-emacs)
  (define-key
    evil-normal-state-map (kbd "C-h") 'ns-do-hide-emacs)
  (define-key
    evil-normal-state-map (kbd "M-h") 'help-for-help)
  (key-chord-define
    evil-normal-state-map "tr" (lambda () (interactive) (open-terminal)))
  (key-chord-define
    evil-normal-state-map "ls" (lambda () (interactive) (buffer-menu)))
  (key-chord-define
    evil-normal-state-map "gt"
      (lambda () (interactive) (advanced-switch-buffer 'next-buffer)))
  (key-chord-define
    evil-normal-state-map "gz"
      (lambda () (interactive) (advanced-switch-buffer 'previous-buffer)))

  (define-key                           ; Company
    company-active-map (kbd "TAB") 'company-select-next-or-abort)
  (define-key
    company-active-map [tab] 'company-select-next-or-abort)
  (define-key
    company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key
    company-active-map (kbd "C-p") 'company-select-previous-or-abort)

  (evil-define-key                      ; NeoTree
    'normal neotree-mode-map (kbd "o") 'neotree-enter)
  (evil-define-key
    'normal neotree-mode-map (kbd "r") 'neotree-refresh)

  (global-set-key                       ; Magit
    (kbd "C-g") (lambda () (interactive) (magit-status)))

  (key-chord-define                     ; General Evil
    evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define
    evil-insert-state-map "JK" 'evil-normal-state)


  (key-chord-mode 1))


(provide 'radek-keybindings)

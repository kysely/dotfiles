(use-package exec-path-from-shell       ; Load $PATH
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(cd                                     ; cd into common projects dir
 "/Users/radek/Documents/Work/PROJECTS")

(fset 'yes-or-no-p 'y-or-n-p)           ; Shorten yes/no to y/n

;; EDITOR DEFAULTS -------------------------------------------------------------
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
(global-hl-line-mode 0)
(desktop-save-mode 1)

;; Since a lot of special characters on a Czech keyboard layout
;; are written using Option key, let's keep it disabled for Emacs
;; bindings and set Meta to Fn and Control keys (both for better UX).
(setq mac-option-modifier nil)
(setq mac-function-modifier 'meta)
(setq mac-control-modifier 'meta)

;; Also set Control to Cmd key to avoid broken pinky
(setq mac-command-modifier 'control)


(provide 'radek-defaults)

(use-package tuareg                     ; OCaml
  :ensure t
  :config
  (use-package merlin
    :ensure t
    :config
    (add-hook 'tuareg-mode-hook 'merlin-mode)))

(use-package rust-mode                  ; Rust
  :ensure t
  :config
  (use-package racer
    :ensure t
    :config
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)))

(use-package js2-mode                   ; JavaScript
  :ensure t
  :config
  (setq js2-strict-missing-semi-warning nil)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

(use-package tide                       ; TypeScript
  :ensure t
  :config
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package lsp-mode                   ; JS & TS
  :ensure t
  :hook (typescript-mode . lsp)
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dockerfile-mode            ; Dockerfile
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package markdown-mode              ; MarkDown
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode                  ; YAML
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package haskell-mode               ; Haskell
  :ensure t)

(use-package cider                      ; Clojure
  :ensure t)

(autoload 'q-mode "q-mode")             ; kdb+/q
(add-to-list 'auto-mode-alist '("\\.[kq]\\'" . q-mode))


(provide 'radek-languages)

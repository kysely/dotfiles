(use-package monokai-theme              ; Default theme
 :ensure t
 :config
 (setq ;; foreground and background
       monokai-foreground     "#FCFCFA"
       monokai-background     "#2D2A2E"
       ;; highlights and comments
       monokai-comments       "#5B595C"
       monokai-emphasis       "#D9D9D7"
       monokai-highlight      "#434144"
       monokai-highlight-alt  "#66D9EF" ; Default
       monokai-highlight-line "#383539"
       monokai-line-number    "#5B595C"
       ;; colours
       monokai-blue           "#78DCE8"
       monokai-cyan           "#78DCE8"
       monokai-green          "#A9DC76"
       monokai-gray           "#353236"
       monokai-violet         "#AB9DF2"
       monokai-red            "#FF6188"
       monokai-orange         "#FC9867"
       monokai-yellow         "#FFD866")
 (load-theme 'monokai t))

(use-package rainbow-identifiers        ; Semantic syntax highlighting
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))

(use-package dimmer                     ; Dim inactive buffers
  :ensure t
  :config
  (setq dimmer-fraction 0.4)
  (dimmer-mode))

(use-package dash-functional            ; Dependency for pretty-fonts & -eshell
  :ensure t)

;; FONT STYLE ------------------------------------------------------------------
(setq-default line-spacing 6)
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 130
                    :weight 'light)

(use-package pretty-fonts
  :config
  (pretty-fonts-add-hook 'prog-mode-hook pretty-fonts-fira-code-alist)
  (pretty-fonts-add-hook 'org-mode-hook  pretty-fonts-fira-code-alist)

  (pretty-fonts-set-fontsets-for-fira-code)
  (pretty-fonts-set-fontsets
   '(("fontawesome"                     ; All-the-icons fontsets
      ;;                         
      #xf07c #xf0c9 #xf0c4 #xf0cb #xf017 #xf101)

     ("all-the-icons"
      ;;    
      #xe907 #xe928)

     ("github-octicons"
      ;;                               
      #xf091 #xf059 #xf076 #xf075 #xe192  #xf016 #xf071)
      )))

;; ESHELL STYLE ----------------------------------------------------------------
(use-package pretty-eshell
  :init
  (progn
    (setq eshell-banner-message "")     ; Default Banner Message

    (setq pretty-eshell-header "\n")    ; Prompt Styling
    (setq pretty-eshell-prompt-string " "))

  :config
  (progn
    (pretty-eshell-section              ; Directory
     esh-dir
     "\xf07c"  ; 
     (abbreviate-file-name (eshell/pwd))
     '(:foreground "#DEB8FF"))

    (pretty-eshell-section              ; Git Branch
     esh-git
     "\xe907"  ; 
     (magit-get-current-branch)
     '(:foreground "#595C61"))

    (pretty-eshell-section              ; Time
     esh-clock
     "\xe192"  ; 
     (format-time-string "%H:%M:%S" (current-time))
     '(:foreground "#595C61"))

    (pretty-eshell-section              ; Prompt Number
     esh-num
     "\xf0c9"  ; 
     (number-to-string pretty-eshell-prompt-num)
     '(:foreground "#595C61"))

    (setq pretty-eshell-funcs (list esh-dir esh-git esh-clock esh-num))))

;; MODE LINE (style & layout) --------------------------------------------------
(defface my-pl-gui-style
  '((t (:foreground "#787875" :background "#353236" :box nil)))
  "My custom GUI powerline face")
(defface my-pl-active-gui-style
  '((t (:foreground "#FCFCFA" :background "#3C393D" :box nil)))
  "My custom GUI powerline face for the first segment")

(defun minimal-powerline-theme ()
  "Set up my custom Powerline with Evil indicators."
  (setq powerline-height 30)
  (setq powerline-display-hud t)
  (setq powerline-default-separator nil)
  (setq powerline-gui-use-vcs-glyph t)
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))
             (seg1 (if active 'my-pl-active-gui-style 'my-pl-gui-style))
             (seg2 'my-pl-gui-style)
             (seg3 'my-pl-gui-style)
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
                        (powerline-narrow seg2 'l)
                        (powerline-raw " " seg2)))
             (rhs (list (powerline-raw global-mode-string seg2 'r)
                        (powerline-vc seg2 'r)
		    (funcall separator-right seg2 seg1)
		    (unless window-system
                          (powerline-raw (char-to-string #xe0a1) seg1 'l))
                        (powerline-raw "%4l" seg1 'l)
                        (powerline-raw ":" seg1 'l)
                        (powerline-raw "%3c" seg1 'r)
                        (when powerline-display-hud
                          (powerline-hud seg1 seg2)))))
        (concat (powerline-render lhs)
                (powerline-fill seg3 (powerline-width rhs))
                (powerline-render rhs)))))))

(use-package powerline
  :ensure t
  :config

  (use-package powerline-evil
    :ensure t)

  (minimal-powerline-theme))


(provide 'radek-gui)

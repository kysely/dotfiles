(use-package doom-themes              ; Default theme
  :ensure t
  :config
  (load-theme 'doom-molokai t))

(use-package dimmer                     ; Dim inactive buffers
  :ensure t
  :config
  (setq dimmer-fraction 0.4)
  (dimmer-mode))

(use-package dash-functional            ; Dependency for pretty-fonts & -eshell
  :ensure t)

;; FONT STYLE ------------------------------------------------------------------
(setq-default line-spacing 7)
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 100
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

;; MODE LINE ----------------------------------------------------------------
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(provide 'radek-gui)

(use-package monokai-theme              ; Default theme
 :ensure t
 :config
 (setq
  monokai-foreground     "#F2F3F7"
  monokai-background     "#27292C"
  monokai-highlight      "#4D4F52"
  monokai-comments       "#595C61"
  monokai-highlight-line "#2E3133"
  monokai-line-number    "#595C61"
  monokai-gray           "#2E3133")
 (load-theme 'monokai t))

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
  '((t (:foreground "#595C61" :background "#2E3033" :box nil)))
  "My custom GUI powerline face")
(defface my-pl-active-gui-style
  '((t (:foreground "#F2F3F7" :background "#2E3033" :box nil)))
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


(provide 'radek-gui)

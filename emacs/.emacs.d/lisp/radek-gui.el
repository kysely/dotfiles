(use-package monokai-theme              ; Default theme
 :ensure t
 :config
 (load-theme 'monokai 1))

(use-package dimmer                     ; Dim unfocused buffers
  :ensure t
  :init (dimmer-mode)
  :config (setq-default dimmer-fraction 0.5))

;; FONT STYLE ------------------------------------------------------------------
(setq-default line-spacing 6)
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 130
                    :weight 'light)

;; MODE LINE (style & layout) --------------------------------------------------
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


(provide 'radek-gui)

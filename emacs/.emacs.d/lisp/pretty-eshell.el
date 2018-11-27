;;; pretty-eshell.el --- Stylish eshell -*- lexical-binding: t; -*-

;;; License:
;; The MIT License (MIT)

;; Copyright (c) 2016-2018 Eric Kaschalk

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(require 'dash)
(require 'dash-functional)
(require 's)

;;; Config

(defvar pretty-eshell-funcs nil
  "List of `pretty-eshell-section' to enable.")

(defvar pretty-eshell-sep "  "
  "String delimits each `pretty-eshell-section'")

(defvar pretty-eshell-section-delim " "
  "String delimits icons and their text.")

(defvar pretty-eshell-header "\n "
  "Initial string composing the eshell prompt.")

(defvar pretty-eshell-prompt-string " "
  "Prompt string, must match builtin `eshell-prompt-regexp'")

(defvar pretty-eshell-prompt-num 0
  "Prompt number for current eshell session.")

;;; Section utilities

(add-hook 'eshell-exit-hook
          (lambda () (setq pretty-eshell-prompt-num 0)))
(advice-add 'eshell-send-input :before
            (lambda (&rest args) (cl-incf pretty-eshell-prompt-num)))

;;; Core

(defmacro with-face (STR &rest PROPS)
  "Return STR propertized with PROPS."
  `(propertize ,STR 'face (list ,@PROPS)))

;;;###autoload
(defmacro pretty-eshell-section (name icon form &rest props)
  "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
  ;; Roundabout way to handle case that
  ;; 1. Form is a variable and
  ;; 2. That variable might not be defined/initialized
  ;; Eg. pyvenv-virtualenv-name not loaded until pyvenv-workon
  `(setq ,name
         (lambda ()
           (when (or (and (symbolp (quote ,form))
                          (bound-and-true-p ,form))
                     (and (not (symbolp (quote ,form)))
                          ,form))
             (-> ,icon
                (concat pretty-eshell-section-delim ,form)
                (with-face ,@props))))))

;;;###autoload
(defun pretty-eshell--acc (acc x)
  "Accumulator for evaluating and concatenating pretty-eshell-sections."
  (--if-let (funcall x)
      (if (s-blank? acc)
          it
        (s-concat acc pretty-eshell-sep it))
    acc))

;;;###autoload
(defun pretty-eshell-prompt-func ()
  "Value for `eshell-prompt-function'."
  (concat pretty-eshell-header
          (-reduce-from 'pretty-eshell--acc "" pretty-eshell-funcs)
          "\n"
          pretty-eshell-prompt-string))

(setq eshell-prompt-function
      'pretty-eshell-prompt-func)

(provide 'pretty-eshell)

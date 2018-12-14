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

(defun open-terminal ()
  "Procedure for opening terminal panel on the right"
  (split-window-horizontally)
  (next-multiframe-window)
  (shrink-window-horizontally 100000)
  (enlarge-window-horizontally 100)
  (eshell))

(defun confirm-kill-emacs (confirm)
  "Ask before quitting Emacs"
  (interactive "cQuit? (y/n) ")
  (if (eq confirm 121)
      (kill-emacs)))

;; BETTER WINDMOVE -------------------------------------------------------------
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


(provide 'radek-functions)

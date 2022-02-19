(defgroup echo-bar nil
  "Display text at the end of the echo area."
  :group 'applications)

(defcustom echo-bar-center-padding 10
  "Minimum number of columns between the left and right aligned text."
  :group 'echo-bar
  :type 'number)

(defcustom echo-bar-overlays nil
  "List of overlays displaying the echo bar contents."
  :group 'echo-bar
  :type 'list)

(defcustom echo-bar-function 'echo-bar-default-function
  "Function that returns the text displayed in the echo bar."
  :group 'echo-bar
  :type 'function)

(define-minor-mode echo-bar-mode
  "Display text at the end of the echo area."
  :global t
  (if echo-bar-mode
      (echo-bar-enable)
    (echo-bar-disable)))

(defun echo-bar-enable ()
  "Turn on the echo bar."
  (interactive)
  (setq echo-bar-overlays nil)
  (dolist (buf '(" *Echo Area 0*" " *Echo Area 1*"))
    (with-current-buffer buf
      (remove-overlays (point-min) (point-max))
      (push (make-overlay (point-min) (point-max) nil nil t)
            echo-bar-overlays))))

(defun echo-bar-disable ()
  "Turn off the echo bar."
  (interactive)
  (mapc 'delete-overlay echo-bar-overlays)
  (setq echo-bar-overlays nil))

(defun echo-bar-set-text (text)
  "Set the text displayed by the echo bar to TEXT."
  (let* ((wid (string-width text))
         (spc (propertize " " 'display `(space :align-to (- right-fringe ,wid))))
         (str (concat spc text)))
    (dolist (o echo-bar-overlays)
      (overlay-put o 'after-string str))))

(defun echo-bar-update ()
  "Get new text to be displayed from `echo-bar-default-function`."
  (interactive)
  (echo-bar-set-text (funcall echo-bar-function)))

(defun echo-bar-default-function ()
  "Default value of `echo-bar-function`.
Displays the date and time in a basic format."
  (format-time-string "%b %d - %H:%M:%S"))

(defgroup echo-bar nil
  "Display text at the end of the echo area."
  :group 'applications)

(defcustom echo-bar-right-padding 2
  "Number of columns between the text and right margin."
  :group 'echo-bar
  :type 'number)

(defvar echo-bar-overlays nil
  "List of overlays displaying the echo bar contents.")

(defcustom echo-bar-function 'echo-bar-default-function
  "Function that returns the text displayed in the echo bar."
  :group 'echo-bar
  :type 'function)

(defcustom echo-bar-update-interval 1
  "Interval in seconds between updating the echo bar contents.

If nil, don't update the echo bar automatically."
  :group 'echo-bar
  :type 'number)

(defvar echo-bar-timer nil
  "Timer used to update the echo bar.")

(defvar echo-bar-text nil
  "The text currently displayed in the echo bar.")

(define-minor-mode echo-bar-mode
  "Display text at the end of the echo area."
  :global t
  (if echo-bar-mode
      (echo-bar-enable)
    (echo-bar-disable)))

(defun echo-bar-enable ()
  "Turn on the echo bar."
  (interactive)
  ;; Disable any existing echo bar to remove conflicts
  (echo-bar-disable)
  
  ;; Create overlays in each echo area buffer
  (dolist (buf '(" *Echo Area 0*" " *Echo Area 1*"))
    (with-current-buffer buf
      (remove-overlays (point-min) (point-max))
      (push (make-overlay (point-min) (point-max) nil nil t)
            echo-bar-overlays)))
  
  ;; Start the timer to automatically update
  (when echo-bar-update-interval
    (run-with-timer 0 echo-bar-update-interval 'echo-bar-update)))

(defun echo-bar-disable ()
  "Turn off the echo bar."
  (interactive)
  ;; Remove echo bar overlays
  (mapc 'delete-overlay echo-bar-overlays)
  (setq echo-bar-overlays nil)

  ;; Remove text from Minibuf-0
  (with-current-buffer " *Minibuf-0*"
    (delete-region (point-min) (point-max)))

  ;; Cancel the update timer
  (cancel-function-timers 'echo-bar-update))

(defun echo-bar-set-text (text)
  "Set the text displayed by the echo bar to TEXT."
  (let* ((wid (+ (string-width text) echo-bar-right-padding))
         (spc (propertize " " 'display `(space :align-to (- right-fringe ,wid)))))
    
    (setq echo-bar-text (concat spc text))
    
    ;; Add the correct text to each echo bar overlay
    (dolist (o echo-bar-overlays)
      (overlay-put o 'after-string echo-bar-text))

    ;; Display the text in Minibuf-0
    (with-current-buffer " *Minibuf-0*"
      (delete-region (point-min) (point-max))
      (insert echo-bar-text))))

(defun echo-bar-update ()
  "Get new text to be displayed from `echo-bar-default-function`."
  (interactive)
  (echo-bar-set-text (funcall echo-bar-function)))

(defun echo-bar-default-function ()
  "Default value of `echo-bar-function`.
Displays the date and time in a basic format."
  (format-time-string "%b %d - %H:%M:%S"))

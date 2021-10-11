(defvar echo-bar--inhibit nil
  "Inhibit displaying the echo bar to avoid recursive loops")

(defvar echo-bar-max-message-length 300
  "Maximum length of message to display at the left side of the echo area")

(defun echo-bar-display (&rest args)
  "Compile and display the echo bar text"
  (interactive)
  (ignore-errors
    (let* ((message-log-max nil)
           (inhibit-read-only t)
           (previous-output (with-current-buffer " *Echo Area 1*" (buffer-string)))
           ;; The right aligned text
           (text (format "%s  |  %s  |  %s"
                         (echo-bar-battery-format)
                         (format-time-string "  %b %d")
                         (format-time-string "  %H:%M")))
           ;; The width in pixels of the right aligned text
           (text-width (save-window-excursion
                         (when (minibufferp nil) (other-window 1))
                         (delete-other-windows)
                         (with-temp-buffer
                           (switch-to-buffer " *temp*")
                           (insert text)

                           ;; Add a bit of padding, so that the ] at the end of the echo in the minibuffer
                           ;; doesn't wrap to the next line
                           (insert "  ")

                           ;; Get the pixel width of the right align text, in case the default font is variable pitch
                           (car (window-text-pixel-size (selected-window) (point-min) (point-max))))))
           ;; The pixel column to align the right aligned text
           (align-column (- (frame-text-width) text-width))
           ;; Space with display property to align the right aligned text to the correct position
           (align-space (propertize "§" 'display `(space . (:align-to (,align-column)))))
           ;; Makes the echo area taller
           (tall-space (propertize " " 'display '((height 1.5) (raise -0.1)))))

      (if (minibufferp nil)
          (progn (message "%s%s" align-space text)
                 (setq-local truncate-lines nil))
        (message "%s%s%s%s" previous-output tall-space align-space text)))))

(defun echo-bar-battery-format ()
  (let* ((status (funcall battery-status-function))
         (percent (round (string-to-number (battery-format "%p" status))))
         (power-method (battery-format "%L" status)))
    (format "%s %s   %s%%"
            (if (string= power-method "AC") "⚡" "")
            (cond ((>= percent 95) "")
                  ((>= percent 70) "")
                  ((>= percent 50) "")
                  ((>= percent 15) "")
                  (t ""))
            percent)))

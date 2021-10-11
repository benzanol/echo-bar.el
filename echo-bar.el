(defvar echo-bar--inhibit nil
  "Inhibit displaying the echo bar to avoid recursive loops")

(defvar echo-bar-max-message-length 300
  "Maximum length of message to display at the left side of the echo area")

(add-hook 'post-command-hook 'echo-bar-display)
(advice-add 'message :after 'echo-bar-display)

(defun echo-bar-display (&rest args)
  "Compile and display the echo bar text"
  (interactive)
  (unless echo-bar--inhibit
    (let* ((message-log-max nil)
           (inhibit-read-only t)
           (echo-bar--inhibit t)
           (echo-0 (with-current-buffer " *Echo Area 0*" (buffer-string)))

           (previous-output
            (with-current-buffer " *Echo Area 1*"
              (push echo-0 echo0s)
              (unless (or (string= echo-0 "") (string= (substring echo-0 0 1) "§"))
                (delete-region (point-min) (point-max)) (insert echo-0))
              (when (and (> (point-max) 1) (string= (buffer-substring 1 2) "§"))
                (delete-region (point-min) (point-max)))
              (buffer-string)))

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
                           (switch-to-buffer (current-buffer))
                           (insert (concat text "  "))
                           (car (window-text-pixel-size (selected-window) (point-min) (point-max))))))

           ;; The pixel column to align the right aligned text
           (align-column (- (frame-text-width) text-width))
           ;; Space with display property to align the right aligned text to the correct position
           (align-space (propertize "§" 'display `(space . (:align-to (,align-column)))))
           ;; Indicates that a particular message is from the echo bar
           (indicator (propertize "§" 'invisible t))
           ;; Makes the echo area taller
           (tall-space (propertize " " 'display '((height 1.5) (raise -0.1)))))
      (setq mywid text-width)
      (setq mycol align-column)

      (if (minibufferp nil)
          (progn (message "%s%s%s" indicator align-space text)
                 (setq-local truncate-lines nil))
        (message "%s%s%s%s%s" indicator previous-output tall-space align-space text)))))

(defun echo-bar-battery-format ()
  "Return a string containing a formatted battery display for the echo bar"
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

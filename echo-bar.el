(defvar echo-bar--inhibit nil
  "Inhibit displaying the echo bar to avoid recursive loops")

(defvar echo-bar-center-padding 100
  "Minimum width in pixels between the left and right aligned text")

(add-hook 'post-command-hook 'echo-bar-display)
(add-hook 'echo-area-clear-hook 'echo-bar-display)

;; Trigger redisplay after any message is sent
(advice-add 'message :after 'echo-bar-display)

;; Trigger redisplay before typing a character in vertico
(advice-add 'vertico--exhibit :before 'echo-bar-display)

(defun echo-bar-display (&rest args)
  "Compile and display the echo bar text"
  (interactive)
  (unless (or echo-bar--inhibit
              (with-current-buffer " *Echo Area 0*"
                (and (> (point-max) 1) (string= (buffer-substring 1 2) "§"))))
    (let* ((message-log-max nil)
           (inhibit-read-only t)
           (echo-bar--inhibit t)
           (echo-0 (with-current-buffer " *Echo Area 0*" (buffer-string)))

           ;; The right aligned text
           (text (format "%s  |  %s  |  %s"
                         (echo-bar-battery-format)
                         (format-time-string "  %b %d")
                         (format-time-string "  %H:%M")))

           ;; The text that would otherwise be displayed in the minibuffer
           (previous-echo
            (with-current-buffer " *Echo Area 1*"
              (push echo-0 echo0s)
              (unless (or (string= echo-0 "") (string= (substring echo-0 0 1) "§"))
                (delete-region (point-min) (point-max)) (insert echo-0))
              (when (and (> (point-max) 1) (string= (buffer-substring 1 2) "§"))
                (delete-region (point-min) (point-max)))
              (buffer-string)))

           ;; Indicates that a particular message is from the echo bar
           (indicator (propertize "§" 'invisible t))

           ;; Makes the echo area taller
           (tall-space (propertize " " 'display '((height 1.5) (raise -0.1))))

           right-width right-column align-space
           left-width left-length left-text y start-x end-x)

      ;; Calculate the pixel lengthts of the left and right aligned text
      (save-window-excursion
        (when (minibufferp nil) (other-window 1))
        (delete-other-windows)
        (with-temp-buffer
          (switch-to-buffer (current-buffer))
          ;; Calculate the width of the right aligned text
          (insert (concat text "  "))
          (setq right-width (car (window-text-pixel-size nil (point-min) (point-max)))
                right-column (- (frame-text-width) right-width)
                align-space (propertize "§" 'display `(space . (:align-to (,right-column)))))

          ;; Calculated the length of the left aligned text
          (setq left-width (- (frame-text-width) right-width echo-bar-center-padding))
          (delete-region (point-min) (point-max))
          (setq start-x (car (posn-x-y (event-start nil)))
                y (cdr (posn-x-y (event-start nil))))
          (insert (substring previous-echo 0 (min (length previous-echo) (- (window-width) 2))))
          (setq end-x (car (posn-x-y (event-start nil))))
          (if (< (- end-x start-x) left-width)
              (setq left-text previous-echo)
            (setq left-length (- (cadr (posn-at-x-y (+ start-x left-width) y)) 4))
            (setq left-text (concat (buffer-substring (point-min) left-length) "...")))))

      (if (minibufferp nil)
          (progn (message "%s%s%s" indicator align-space text)
                 (setq-local truncate-lines nil))
        (message "%s%s%s%s%s" indicator left-text tall-space align-space text)))))

(make-string 500 ?a)

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

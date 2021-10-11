(defvar echo-bar--inhibit nil
  "Inhibit displaying the echo bar to avoid recursive loops")

(defvar echo-bar-center-padding 10
  "Minimum number of columns between the left and right aligned text")

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
           (right-text (funcall echo-bar-function))

           (align-position (- (window-text-width (cadr (window-tree))) (length right-text)))
           (align-space (propertize "§" 'display `(space :align-to ,(- align-position 2))))

           ;; The text that would otherwise be displayed in the minibuffer
           (previous-echo
            (with-current-buffer " *Echo Area 1*"
              (unless (or (string= echo-0 "") (string= (substring echo-0 0 1) "§"))
                (delete-region (point-min) (point-max)) (insert echo-0))
              (when (and (> (point-max) 1) (string= (buffer-substring 1 2) "§"))
                (delete-region (point-min) (point-max)))
              (buffer-string)))

           (left-max-width (- (window-text-width (cadr (window-tree)))
                              (length right-text) echo-bar-center-padding))

           (left-text (substring previous-echo 0 (min (length previous-echo) left-max-width)))

           ;; Indicates that a particular message is from the echo bar
           (indicator (propertize "§" 'invisible t))

           ;; Makes the echo area taller
           (tall-space (propertize " " 'display '((height 1.5) (raise -0.1)))))


      (if (minibufferp nil)
          (progn (message "%s%s%s" indicator align-space right-text)
                 (setq-local truncate-lines nil))
        (message "%s%s%s%s%s" indicator left-text tall-space align-space right-text)))))


(add-hook 'post-command-hook 'echo-bar-display)
(add-hook 'echo-area-clear-hook 'echo-bar-display)

;; Trigger redisplay after any message is sent
(advice-add 'message :after 'echo-bar-display)

;; Trigger redisplay before typing a character in vertico
(advice-add 'vertico--exhibit :before 'echo-bar-display)


(defvar echo-bar-function 'echo-bar-default-function
  "Function that returns the text displayed in the echo bar")

(defun echo-bar-default-function ()
  "Default value of `echo-bar-function`
Displays the date and time in a basic format."
  (format-time-string "%b %d - %H:%M"))



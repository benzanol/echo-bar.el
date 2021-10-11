(defvar echo-bar--inhibit nil
  "Inhibit displaying the echo bar to avoid recursive loops")

(defvar echo-bar-center-padding 10
  "Minimum number of columns between the left and right aligned text")

(defvar echo-bar-timer (run-with-timer 0 0.01 'echo-bar-display)
  "Redraw the echo bar repeatedly")

(defun echo-bar-calculate-text ()
  (let* ((right-text (funcall echo-bar-function))

         (align-position (- (window-text-width (cadr (window-tree))) (length right-text)))
         (align-space (propertize "§" 'display `(space :align-to ,(- align-position 2))))

         ;; The text that would otherwise be displayed in the minibuffer
         (echo-0 (with-current-buffer " *Echo Area 0*" (buffer-string)))
         (previous-echo
          (with-current-buffer " *Echo Area 1*"
            (unless (or (string= echo-0 "") (string= (substring echo-0 0 1) "§"))
              (delete-region (point-min) (point-max)) (insert echo-0))
            (when (and (> (point-max) 1) (string= (buffer-substring 1 2) "§"))
              (delete-region (point-min) (point-max)))
            (buffer-string)))

         (left-max-width (- (window-text-width (cadr (window-tree)))
                            (length right-text) echo-bar-center-padding))
         (left-text (substring previous-echo 0 (min (length previous-echo) left-max-width))))
    (list :left left-text :right right-text :space align-space)))

(defun echo-bar-display (&rest args)
  "Compile and display the echo bar text"
  (interactive)
  (unless (or echo-bar--inhibit
              (minibufferp nil)
              (with-current-buffer " *Echo Area 0*"
                (and (> (point-max) 1) (string= (buffer-substring 1 2) "§"))))
    (let* ((message-log-max nil)
           (inhibit-read-only t)
           (echo-bar--inhibit t)
           (text-plist (echo-bar-calculate-text)))
      (message "%s%s%s%s%s"
               (propertize "§" 'invisible t)
               (plist-get text-plist :left)
               (propertize " " 'display '((height 1.5) (raise -0.1)))
               (plist-get text-plist :space)
               (plist-get text-plist :right)))))


(add-hook 'post-command-hook 'echo-bar-display)

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



(defvar echo-bar--inhibit nil
  "Inhibit displaying the echo bar to avoid recursive loops")

(defvar echo-bar-center-padding 10
  "Minimum number of columns between the left and right aligned text")

(defvar echo-bar-fast-timer (run-with-timer 0 0.01 'echo-bar-timer-display)
  "Constantly check whether the echo bar needs to be updated")

(defvar echo-bar-slow-timer (run-with-timer 0 1 'echo-bar-display)
  "Always update the echo bar every minute")

;; Update the echo bar after every command
(add-hook 'post-command-hook 'echo-bar-display)

;; Update the echo bar after any message is sent
(advice-add 'message :after 'echo-bar-display)

;; Called by the fast timer many times per second
(defun echo-bar-timer-display (&rest args)
  "Display the echo bar only if it isn't already shown,
and the minibuffer isn't active."
  (ignore-errors
    (unless echo-bar--inhibit
      (let* ((echo-bar--inhibit t)
             (message-log-max nil)
             (inhibit-read-only t))
        (unless (minibufferp nil)
          (if (with-current-buffer " *Echo Area 0*"
                (and (> (point-max) 1) (string= (buffer-substring 1 2) "§")))
              (message (with-current-buffer " *Echo Area 0*" (buffer-string)))
            (echo-bar-normal-display)))))))

(defun echo-bar-display (&rest args)
  "Display the echo bar either in the minibuffer, or in the echo area"
  (ignore-errors
    (unless echo-bar--inhibit
      (let* ((echo-bar--inhibit t)
             (message-log-max nil)
             (inhibit-read-only t))
        (if (minibufferp nil)
            (echo-bar-minibuffer-display)
          (echo-bar-normal-display))))))


(defun echo-bar-normal-display ()
  "Display the echo bar in the echo area"
  (interactive)
  (let ((text-plist (echo-bar-text-plist)))
    (message "%s%s%s%s%s"
             (propertize "§" 'invisible t)
             (plist-get text-plist :left)
             (propertize " " 'display '((height 1.5) (raise -0.1)))
             (plist-get text-plist :space)
             (plist-get text-plist :right))))

(defun echo-bar-minibuffer-display ()
  "Display the echo bar in the minibuffer"
  (let* ((text-plist (echo-bar-text-plist)))
    (message "%s[%s]"
             ;; Decrease the align property by 3
             (--> (plist-get text-plist :space)
               (get-text-property 0 'display it)
               (- (caddr it) 3)
               (list 'space :align-to it)
               (propertize " " 'display it))
             (plist-get text-plist :right))))

(defun echo-bar-text-plist ()
  "Figure out the text to be displayed the echo bar"
  (let* ((right-text (funcall echo-bar-function))
         (align-position (- (window-text-width (cadr (window-tree))) (length right-text)))
         (align-space (propertize " " 'display `(space :align-to ,align-position)))

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


;; User customization

(defvar echo-bar-function 'echo-bar-default-function
  "Function that returns the text displayed in the echo bar")

(defun echo-bar-default-function ()
  "Default value of `echo-bar-function`
Displays the date and time in a basic format."
  (format-time-string "%b %d - %H:%M:%S"))



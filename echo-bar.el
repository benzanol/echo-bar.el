(defvar echo-bar--inhibit nil
  "Inhibit displaying the echo bar to avoid recursive loops")

(defvar echo-bar-max-message-length 300
  "Maximum length of message to display at the left side of the echo area")

(defun echo-bar-display (&rest args)
  (unless echo-bar--inhibit
    (let* ((echo-bar--inhibit t)
           (message-log-max nil)
           (inhibit-read-only t)
           (previous-contents
            (replace-regexp-in-string
             "^⟨ \\(.*\\) ⟩.*$" "\\1"
             (with-current-buffer " *Echo Area 0*"
               (buffer-substring (point-min) (point-max)))))
           (left-text
            (format "%s %s %s"
                    #("⟨" 0 1 (invisible t))
                    (substring
                     previous-contents 0
                     (min (length previous-contents)
                          echo-bar-max-message-length))
                    #("⟩" 0 1 (invisible t))))
           (right-text
            (format "%s  |  %s  |  %s"
                    (qv/battery-format)
                    (format-time-string "  %b %d")
                    (format-time-string "  %H:%M")))
           (right-text-width
            (save-window-excursion
              (switch-to-buffer " *Echo Area 0*")
              (delete-other-windows)
              (delete-region (point-min) (point-max))
              (insert right-text)
              (car (window-text-pixel-size nil (point-min) (point-max)))))

           (align-column (- (window-pixel-width (cadr (window-tree))) right-text-width))
           (align-space (propertize " " 'display `(space . (:align-to (,align-column)))))

           (tall-space (propertize " " 'display '((raise -0.15) (height 1.5)))))

      (if (minibufferp (current-buffer))
          (message "%s%s" align-space right-text)
        (message "%s%s%s" left-text align-space right-text)))))

(defun qv/battery-format ()
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

(add-hook 'post-command-hook 'echo-bar-display)

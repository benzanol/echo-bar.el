;;; echo-bar.el --- Turn the echo area into a custom status bar  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022  Adam Tillou

;; Author: Adam Tillou <qaiviq@gmail.com>
;; Keywords: convenience, tools
;; Version: 1.0.0
;; Homepage: https://github.com/qaiviq/echo-bar.el

;; Note: This package will work without lexical binding, so there is no
;; Emacs 24 requirement.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This package will allow you to display a custom status at the end
;; of the echo area, like polybar but inside of Emacs.

;; To install, just run `M-x package-install RET echo-bar RET`.
;; To customize the text that gets displayed, set the variable
;; `echo-bar-function` to the name of your own custom function.
;; To turn the echo bar on or off, use `echo-bar-mode`.


;;; Code:

(require 'minibuffer)
(require 'overlay)
(require 'seq)
(require 'timer)


(defgroup echo-bar nil
  "Display text at the end of the echo area."
  :group 'applications)

(defcustom echo-bar-function #'echo-bar-default-function
  "Function that returns the text displayed in the echo bar."
  :group 'echo-bar
  :type 'function)

(defcustom echo-bar-format
  '(:eval (format-time-string "%b %d | %H:%M:%S"))
  "Format of the text displayed in the echo bar.

This format will only apply if `echo-bar-function' is set to
`echo-bar-default-function', otherwise, the output of
`echo-bar-function' will be used.

See `mode-line-format' for more info about the required format."
  :group 'echo-bar
  :type 'sexp)

(defcustom echo-bar-right-padding 2
  "Number of columns between the text and right margin."
  :group 'echo-bar
  :type 'number)

(defcustom echo-bar-minibuffer t
  "If non-nil, also display the echo bar when in the minibuffer."
  :group 'echo-bar
  :type 'boolean)

(defcustom echo-bar-update-interval 1
  "Interval in seconds between updating the echo bar contents.

If nil, don't update the echo bar automatically."
  :group 'echo-bar
  :type 'number)

(defcustom echo-bar-per-frame nil
  "Display different echo-bar content on every frame.

The `echo-bar-function' will be called once on each frame.

Re-run `echo-bar-enable' must be re-run after setting this variable to
ensure that it takes effect. Additionally, `echo-bar-enable' must be
re-run each time a frame is added."
  :group 'echo-bar
  :type 'boolean)

(defcustom echo-bar-frame-predicate #'echo-bar-default-frame-predicate
  "Only create per-frame overlays for frames satisfying this predicate.

This is only relevant when `echo-bar-per-frame' is non-nil."
  :group 'echo-bar
  :type 'function)

(defvar echo-bar-timer nil
  "Timer used to update the echo bar.")

(defvar echo-bar-text nil
  "The text currently displayed in the echo bar.")

(defvar echo-bar-minibuffer-overlay nil)

(defvar echo-bar-overlays nil
  "Alist mapping frames to overlays displaying the echo bar contents.

If `echo-bar-per-frame' is `nil', then the alist will have a single
entry with key `t', that will be used on all frames.")

(defmacro echo-bar-with-no-redisplay (&rest body)
  "Execute BODY without any redisplay execution."
  (declare (indent 0) (debug t))
  `(let ((inhibit-read-only t)
         (inhibit-redisplay t)
         (inhibit-modification-hooks t)
         (cursor-sensor-inhibit t)
         after-focus-change-function
         buffer-list-update-hook
         display-buffer-alist
         window-configuration-change-hook
         window-scroll-functions
         window-size-change-functions
         window-state-change-hook)
     ,@body))


;;;###autoload

(define-minor-mode echo-bar-mode
  "Display text at the end of the echo area."
  :global t
  (if echo-bar-mode
      (echo-bar-enable)
    (echo-bar-disable)))


;;;###autoload

(defun echo-bar-enable ()
  "Turn on the echo bar."
  (interactive)
  ;; Disable any existing echo bar to remove conflicts
  (with-demoted-errors "Error disabling echo bar: %s"
    (echo-bar-disable))
  (setq echo-bar-mode t)

  ;; Create overlays for each relevant frame. If `echo-bar-per-frame' is nil,
  ;; then only create 1 set of overlays
  ;; Remove old overlays
  (dolist (buf (mapcar #'get-buffer-create '(" *Echo Area 0*" " *Echo Area 1*" " *Minibuf-0*")))
    (with-current-buffer buf (remove-overlays (point-min) (point-max))))
  (let* ((frames (if (not echo-bar-per-frame) (list t)
                   (seq-filter echo-bar-frame-predicate (frame-list)))))
    (setq echo-bar-overlays (mapcar (lambda (f) (list f nil nil nil)) frames)))
  (echo-bar--ensure-overlays)

  ;; Start the timer to automatically update
  (when echo-bar-update-interval
    (run-with-timer 0 echo-bar-update-interval 'echo-bar-update))
  (echo-bar-update) ;; Update immediately

  ;; Add the setup function to the minibuffer hook
  (add-hook 'minibuffer-setup-hook #'echo-bar--minibuffer-setup))


(defun echo-bar--ensure-overlays ()
  "Ensure that all of the correct overlays exist.

`echo-bar-overlays' should be a list of (frame EA0-OL EA1-OL) where
EA0-OL is in *Echo Area 0*, and EA1-OL is in *Echo Area 1*."
  (pcase-dolist (`(,idx . ,buf) '((1 . " *Echo Area 0*") (2 . " *Echo Area 1*") (3 . " *Minibuf-0*")))
    (with-current-buffer buf
      (dolist (frame-ols echo-bar-overlays)
        (unless (and (nth idx frame-ols) (overlay-buffer (nth idx frame-ols)))
          (setf (nth idx frame-ols)
                (echo-bar--new-overlay (car frame-ols) (current-buffer))))))))


;;;###autoload

(defun echo-bar-disable ()
  "Turn off the echo bar."
  (interactive)
  ;; Remove echo bar overlays
  (echo-bar-with-no-redisplay
    (dolist (frame-ols echo-bar-overlays)
      (dolist (ol (cdr frame-ols))
        (when (overlay-buffer ol) (delete-overlay ol))))
    (setq echo-bar-overlays nil)
    (setq echo-bar-mode nil))

  ;; Cancel the update timer
  (cancel-function-timers #'echo-bar-update)

  ;; Remove the setup function from the minibuffer hook
  (remove-hook 'minibuffer-setup-hook #'echo-bar--minibuffer-setup))


(defun echo-bar--string-pixel-width (str)
  "Return the width of STR in pixels."

  ;; Make sure the temp buffer settings match the minibuffer settings
  (with-selected-window (minibuffer-window)
    (if (fboundp #'string-pixel-width)
        (string-pixel-width str)
      (require 'shr)
      (shr-string-pixel-width str))))

(defun echo-bar--str-len (str)
  "Calculate STR in pixel width."
  (let ((width (frame-char-width))
        (len (echo-bar--string-pixel-width str)))
    (+ (/ len width)
       (if (zerop (% len width)) 0 1))))  ; add one if exceeed

(defun echo-bar-set-text (text-or-fn)
  "Set the text displayed by the echo bar to TEXT-OR-FN.

TEXT-OR-FN is either a string, or a 0-argument function (that will be
called in each frame) to generate a per-frame string."

  ;; Loop over all of the frame entries
  (dolist (frame-ols echo-bar-overlays)
    (with-selected-frame (or (when (frame-live-p (car frame-ols)) (car frame-ols))
                             default-minibuffer-frame (selected-frame))

      (let* ((text (if (stringp text-or-fn) text-or-fn (funcall text-or-fn)))
             (wid (+ (echo-bar--str-len text) echo-bar-right-padding
                     (if (and (display-graphic-p)
                              (> (nth 1 (window-fringes)) 0)
                              (not overflow-newline-into-fringe)
                              (<= echo-bar-right-padding 0))
                         1
                       0)))
             ;; Maximum length for the echo area message before wrap to next line
             (max-len (- (frame-width) wid 5))
             ;; Align the text to the correct width to make it right aligned
             (spc (propertize " " 'cursor 1 'display
                              `(space :align-to (- right-fringe ,wid)))))

        ;; Re-create the overlays if they were deleted
        (echo-bar--ensure-overlays)

        (setq echo-bar-text (concat spc text))

        (when (and echo-bar-minibuffer-overlay (overlay-buffer echo-bar-minibuffer-overlay)
                   (eq (selected-window) (minibuffer-window (selected-frame))))
          (overlay-put echo-bar-minibuffer-overlay 'after-string echo-bar-text))

        ;; Add the correct text to each echo bar overlay
        (dolist (ol (cdr frame-ols))
          (when (overlay-buffer ol)

            (with-current-buffer (overlay-buffer ol)
              ;; Wrap the text to the next line if the echo bar text is too long
              (if (> (mod (point-max) (frame-width)) max-len)
                  (overlay-put ol 'after-string (concat "\n" echo-bar-text))
                (overlay-put ol 'after-string echo-bar-text)))))))))

(defun echo-bar--new-overlay (frame buffer &optional remove-dead)
  "Add a new echo-bar overlay for FRAME in BUFFER.

When REMOVE-DEAD is non-nil, also remove any dead overlays, i.e., those
without a buffer."
  (when remove-dead
    ;; Remove all dead overlays from the list
    (dolist (entry echo-bar-overlays)
      (setcdr entry (seq-filter 'overlay-buffer (cdr entry)))))

  (let* ((ol (make-overlay (point-max) (point-max) (get-buffer buffer) t t)))
    ;; Make the overlay only visible on one frame
    (when echo-bar-per-frame
      (overlay-put ol 'window (minibuffer-window (when (frame-live-p frame) frame))))
    ol))

(defun echo-bar--minibuffer-setup ()
  "Setup the echo bar in the minibuffer."
  (when echo-bar-minibuffer
    (setq echo-bar-minibuffer-overlay (echo-bar--new-overlay t (current-buffer) t))
    (overlay-put echo-bar-minibuffer-overlay 'priority 1)
    (echo-bar-update)))

(defun echo-bar-update ()
  "Get new text to be displayed from `echo-bar-default-function`."
  (interactive)
  (when echo-bar-mode
    (echo-bar-with-no-redisplay
      (echo-bar-set-text echo-bar-function))))

(defun echo-bar-default-function ()
  "The default function to use for the contents of the echo bar.
Returns the formatted text from `echo-bar-format'."
  (format-mode-line echo-bar-format))

(defun echo-bar-default-frame-predicate (frame)
  "Default `echo-bar-frame-predicate'.

Excludes child frames (posframe, mini-frame, company-box, etc.)."
  (not (frame-parameter frame 'parent-frame)))

(defun echo-bar-after-make-frame-function (_frame)
  "Run this hook after creating a new frame."
  ;; Create overlays for the new frame
  (when (and echo-bar-mode echo-bar-per-frame)
    (echo-bar-enable)))

(add-hook 'after-make-frame-functions #'echo-bar-after-make-frame-function)

(provide 'echo-bar)


;;; echo-bar.el ends here

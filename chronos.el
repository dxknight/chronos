;;; chronos.el --- multiple simultaneous countdown / countup timers

;; Copyright (C) 2015 David Knight

;; Author: David Knight <dxknight@opmbx.org>
;; Created: 12 May 2015
;; Package-Version: 1.0
;; Version: 1.0
;; Keywords: calendar
;; URL: http://github.com/...

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Chronos provides multiple countdown / countup timers, shown sorted by expiry
;; time in a special buffer *chronos*.
;;
;;    Expiry      Elapsed      To go  Message                  
;;    [17:02]                         --now--
;;    [17:07]          21       4:51  Coffee
;;
;; Here, a five minute countdown timer was set up 21 seconds ago.  It is
;; expected to expire in 4 minutes 51 seconds at 17:07.  The time 'now' is
;; 17:02.
;;
;;    Expiry      Elapsed      To go  Message                  
;;    [17:13]          45         45+ Test run number 3
;;    [17:13]                         --now--
;;
;; Here, a countup timer was started 45 seconds ago to time Test run number 3.
;;
;; Installation.
;;
;; Put this file somewhere Emacs can find it and (require 'chronos).  Running
;; M-x chronos-add-timer will initialize chronos if required before it adds the
;; first timer.
;;
;; No customization is required but you may wish to bind `chronos-add-timer',
;; set the notification function `chronos-action-function' and change the faces
;; used in the *chronos* buffer.  In my init file (which uses the excellent
;; use-package) I set notification to be a bell played by mpv and a temporary
;; message shown in the buffer, and bind the add command to (C-c t):
;;
;;   (use-package chronos
;;     :init      (progn
;;                  (setq chronos-shell-notify-command "mpv --really-quiet \
;;  --af=scaletempo=speed=pitch --speed=0.65 ~/wip/progs/cdt/temple-bell-zen.mp3")
;;                  (setq chronos-action-function '(lambda (c)
;;                                                   (chronos-shell-notify c)
;;                                                   (chronos-buffer-notify c))))
;;     :bind      ("C-c t" . chronos-add-timer))
;;
;; A chrono timer can be added from anywhere in Emacs using the command: M-x
;; chronos-add-timer and providing an expiry time specification and a message.
;; In the chronos buffer, key a is bound to chronos-add-timer.
;;
;; Expiry time specification.
;;
;; The expiry time specification can be relative to current time or the expiry
;; of another timer, or absolute.
;;
;; - absolute time: start with an =, followed by 24hr clock.  For example,
;;                  =17:00 is an expiry time of five o'clock in the afternoon.
;;
;; - relative time: one number indicates minutes in the future, two indicates
;;                  hours and minutes and three indicates hours minutes and
;;                  seconds.  Positive numbers indicate later expiry, negative
;;                  ones earlier.
;;
;;                  For example, if the current time is 17:00:
;;                  5 gives an expiry time of 17:05
;;                  1:30 gives 18:30
;;                  0:0:30 gives 30 seconds after 17:00
;;                  0 gives a count up timer.
;;
;;                  Negative relative times are more useful against existing
;;                  chronos.  Here, a chrono was set for the absolute time
;;                  19:00, then with the cursor on this chrono and using 'C-u a'
;;                  to add a relative chrono, two extra notifications were set
;;                  for -5 (five minutes before end, 18:55) and -15 (fifteen
;;                  minutes before end, 18:45).
;;
;;                  Expiry      Elapsed      To go  Message                  
;;                  [18:04]                         --now--
;;                  [18:45]          45      41:10  Any questions
;;                  [18:55]          58      51:10  Thanks and goodbyes
;;                  [19:00]        2:01      56:10  Talk ends
;;
;; Controls.
;;
;; Each chrono can be paused/unpaused, adjusted or deleted.  Default keybindings
;; in the *chronos* buffer are:
;;
;; SPC - pause/unpause (pausing affects time to go and the expiry time, but not
;;       elapsed time)
;; a   - add a timer.
;; n/p - move selection down/up
;; d   - delete selected timer
;; e   - edit selected timer
;; l   - lap selected timer
;; q   - quit window
;; Q   - prompt if any timers exist, kill the *chronos* buffer and clean up.
;;
;; Whether relative times are against current or a selected chrono is controlled
;; by the prefix.
;;
;; - Adding a timer with (a) is usually relative to current time; (C-u a) will
;;   calculate relative times against the selected timer.
;;
;; - Editing (adjusting) the selected timer with (e) will calculate relative
;;   times against the currently set expiry time of the timer.  (C-u e) will
;;   calculate relative times against current.
;;
;; Notifications.
;;
;; By default, expired timers are shown in the *chronos* buffer above the
;; --now-- line until they are deleted.  Additional actions can be set for when
;; a timer expires by setting `chronos-action-function' to a custom function,
;; perhaps referring to:
;;
;; - A temporary notification in the *chronos* buffer, shown in a customizable
;;   face for a customizable period. `chronos-buffer-notify'
;;
;; - Running a shell command to e.g. ring a bell.  `chronos-shell-notify-command' and `chronos-shell-notify'
;;
;; - Notifying in the mode line. `chronos-modeline-notify'
;;
;; - Using an external notification daemon, e.g. dunstify.  `chronos-dunstify'
;;
;; Dependencies.
;;
;; Emacs: Tested with GNU Emacs 24.4.1 (i686-pc-linux-gnu, X toolkit), but
;;        doesn't use anything particularly fancy so should work on other
;;        versions.
;;
;; Not required but useful:
;;
;; Some means of producing a notification sound (an example is given for MPV and
;; a bell mp3).
;;
;; A desktop wide notification daemon (an example is given for dunstify).

;;; Code:

(defgroup chronos nil
  "Chronos' customization group"
  :group 'calendar)

(defface chronos-default
  '((t (:inherit default)))
  "Basic face for chrono display."
  :group 'chronos)

(defface chronos-now
  '((t (:inherit bold)))
  "Face for showing the current time."
  :group 'chronos)

(defface chronos-selected
  '((t (:inherit highlight)))
  "Face for selected timer."
  :group 'chronos)

(defface chronos-expired
  '((t (:inherit warning)))
  "Face for expired (counted down to zero, now counting how long ago) timers."
  :group 'chronos)

(defface chronos-paused
  '((t (:inherit shadow)))
  "Face for paused timers."
  :group 'chronos)

(defface chronos-header
  '((t (:inherit underline :weight bold)))
  "Face for the header line."
  :group 'chronos)

(defface chronos-notification
  '((t (:inherit warning :height 8.0)))
  "Face for in-window notifications."
  :group 'chronos)

(defface chronos-notification-clock
  '((t (:inherit bold :height 15.0)))
  "Face for in-window notifications."
  :group 'chronos)

(defvar chronos-buffer-name "*chronos*"
  "Buffer name for the chronos buffer")

(defvar chronos-now-message "--now--"
  "Message to place on the 'now' line")

(defvar chronos-header-text "Expiry      Elapsed      To go  Message                  "
  "Header text for the chronos buffer")

(defvar chronos-header-length 1
  "How many lines in the chronos buffer header")

(defvar chronos-action-function nil
  "A function taking a chrono as argument, called when the chrono
  expires.  Nil means no action.")

(defvar chronos-shell-notify-command nil
  "A shell command run when a chrono expires to, for example,
  ring a bell.  Nil means no shell command is run.")

(defvar chronos--buffer nil
  "The special buffer for displaying timers.")

(defvar chronos--timers-list nil
  "The list of timers.")

(defvar chronos-notification-time 15
  "How many seconds to show a notification in buffer.  0 means do
  not show notifications in buffer.")

(defvar chronos-notification-bullet-indent " * "
  "Text to use to bullet/indent notifications.")

(defvar chronos--notification-list nil
  "List of notifications to display in buffer notification area.")

(defvar chronos--update-timer nil
  "A run at time timer for updating the *chronos* buffer with
  chronos--update-display.")

(define-derived-mode chronos-mode special-mode
  "Chronos")

(defun chronos-initialize ()
  "Initialize chronos and switch to the chronos buffer."
  (interactive)
  (setq chronos--buffer (get-buffer-create chronos-buffer-name)
        chronos--timers-list (list (chronos--make-timer nil chronos-now-message))
        chronos--notification-list nil
        chronos--update-timer (run-at-time t 1 'chronos--update-display))
  (switch-to-buffer chronos--buffer)
  (chronos-mode))

(defun chronos-kill ()
  "Clean up and kill chronos."
  (interactive)
  (when (or (< (length chronos--timers-list) 2)
            (y-or-n-p "Chronos are still running.  Do you really want to quit?"))
    (cancel-timer chronos--update-timer)
    (kill-buffer chronos--buffer)
    (setq chronos--timers-list nil
          chronos--notification-list nil
          chronos--update-timer nil
          chronos--buffer nil)))

(define-key chronos-mode-map (kbd "a")   'chronos-add-timer)
(define-key chronos-mode-map (kbd "SPC") 'chronos-toggle-pause-selected-line)
(define-key chronos-mode-map (kbd "e")   'chronos-edit-selected-line)
(define-key chronos-mode-map (kbd "d")   'chronos-delete-selected-line)
(define-key chronos-mode-map (kbd "l")   'chronos-lap-selected-line)
(define-key chronos-mode-map (kbd "D")   'chronos-delete-all-expired)
(define-key chronos-mode-map (kbd "Q")   'chronos-kill)
(define-key chronos-mode-map (kbd "n")   'chronos-next-line)
(define-key chronos-mode-map (kbd "p")   'chronos-previous-line)

;; Timer objects
;;
;; A timer is represented by a list starting with the keyword 'chronos-timer followed
;; by TIME, MSG, ACTION, START
;;
;; TIME is
;;
;; * nil : for the 'now' line that represents the current time
;;
;; * a float : for a paused timer, representing the seconds to go to (+) or past
;;             (-) expiry when it was paused.
;;
;; * a 4 int list : for a running timer, representing the expiry time in
;;                  standard emacs time format.
;;
;; MESSAGE is a string, used for labelling and notification
;;
;; ACTION is a boolean, whether there should be an action on expiry of the
;; timer.
;;
;;   when a running timer has negative time-to-go/gone and :action is t:
;;   * perform action
;;   * set action to nil
;;
;;   when a running or paused timer has time-to-go/gone adjusted from -ve to +ve
;;   * set action to t
;;
;;   when a running or paused timer has time-to-go/gone adjusted from +ve to -ve
;;   * set action to nil
;;
;; START is the 4 int list creation time of the timer, or nil for the 'now'
;; line.

(defun chronos--make-timer (expiry-time message &optional start)
  "Make a new timer object labled with MESSAGE that expires at
EXPIRY-TIME.  The action flag will be set to true if time to
expiry is positive, otherwise nil."
  (list 'chronos-timer
        expiry-time
        message
        (and expiry-time
             (> (float-time (time-subtract expiry-time
                                           (current-time)))
                0))
        (and expiry-time
             (or start
                 (current-time)))))

(defun chronos--copy-timer (c)
  "Make a copy of timer C."
  (copy-sequence c))

(defun chronos--timerp (c)
  "Return t if C is a timer."
  (eq (car c) 'chronos-timer))

(defun chronos--raw-time (c)
  "Return raw time field of timer C."
  (nth 1 c))

(defun chronos--set-raw-time (c time)
  "Set raw time field of timer C to TIME."
  (setf (nth 1 c) time))

(defun chronos--message (c)
  "Return message field of timer C."
  (nth 2 c))

(defun chronos--set-message (c msg)
  "Set message field of timer C to MSG."
  (setf (nth 2 c) msg))

(defun chronos--actionp (c)
  "Return whether there should be an(other) action from chrono
  C."
  (nth 3 c))

(defun chronos--set-action (c a)
  "Set whether (A = nil => no, otherwise yes) there should be
  an(other) action from timer C."
  (setf (nth 3 c) a))

(defun chronos--start-time (c)
  "Return start time of timer C."
  (nth 4 c))

(defun chronos--set-start-time (c time)
  "Set start time of timer C to TIME."
  (setf (nth 4 c) time))

(defun chronos--runningp (c)
  "Return t if C is a running timer."
  (and (chronos--timerp c)
       (listp (chronos--raw-time c))
       (> (length (chronos--raw-time c)) 1)))

(defun chronos--pausedp (c)
  "Return t if C is a paused timer."
  (and (chronos--timerp c)
       (numberp (chronos--raw-time c))))

(defun chronos--running-or-paused-p (c)
  "Return t if timer C is running or paused."
  (or (chronos--runningp c)
      (chronos--pausedp c)))

(defun chronos--nowp (c)
  "Return t if C is the now timer."
  (and (chronos--timerp c)
       (null (chronos--raw-time c))))

(defun chronos--expiry-time (c)
  "Return a 4 int list time that timer C is expected to/did
expire."
  (let ((time (chronos--raw-time c)))
    (cond
     ((chronos--nowp c)     (current-time))
     ((chronos--runningp c) time)
     ((chronos--pausedp c)  (time-add (seconds-to-time time)
                                      (current-time)))
     (t nil))))

(defun chronos--seconds-to-expiry (c)
  "Return a float of seconds until (+ve) or since (-ve) chrono
C's expected expiry."
  (let ((time (chronos--raw-time c)))
    (cond
     ((chronos--nowp c)     0)
     ((chronos--runningp c) (float-time (time-subtract time
                                                       (current-time))))
     ((chronos--pausedp c)  time)
     (t nil))))

(defun chronos--seconds-since-start (c)
  "Return a float of seconds since timer C was created."
  (if (chronos--nowp c)
      0
    (float-time (time-subtract (current-time) (chronos--start-time c)))))

(defun chronos--pause (c)
  "Pause timer C."
  (when (chronos--runningp c)
    (chronos--set-raw-time c
                           (chronos--seconds-to-expiry c))))

(defun chronos--toggle-pause (c)
  "Pause if timer C is running, unpause it if it is paused."
  (cond
   ((chronos--runningp c)
    (chronos--set-raw-time c
                           (chronos--seconds-to-expiry c)))
   ((chronos--pausedp c)
    (chronos--set-raw-time c
                           (chronos--expiry-time c)))))

(defun chronos--expires-earlier-than-p (c1 c2)
  "True if timer C1 expires before timer C2."
  (< (chronos--seconds-to-expiry c1)
     (chronos--seconds-to-expiry c2)))

(defun chronos--expiredp (c)
  "Return whether timer C is expired."
  (> 0 (chronos--seconds-to-expiry c)))

(defun chronos--set-expiry-time (c time)
  "Set timer C to expire at time TIME."
  (chronos--set-raw-time c
                         (cond
                          ((chronos--nowp c)
                           nil)
                          ((chronos--runningp c)
                           time)
                          ((chronos--pausedp c)
                           (float-time
                            (time-subtract time
                                           (current-time)))))))

(defun chronos--set-seconds-to-expiry (c secs)
  "Set timer C so that seconds to expiry is SECS."
  (chronos--set-raw-time
   c
   (cond
    ((chronos--nowp c)     nil)
    ((chronos--runningp c) (time-add (seconds-to-time secs)
                                     (current-time)))
    ((chronos--pausedp c)  secs))))

(defun chronos--check-to-call-action-function (c)
  "Funcall chronos-action-function if required by timer C expiring."
  (when (and chronos-action-function
             (chronos--actionp c)
             (chronos--runningp c)
             (chronos--expiredp c))
    (chronos--set-action c nil)
    (funcall chronos-action-function c)))

(defun chronos--time-string-rounded-to-minute (time)
  "Format TIME rounded to nearest minute"
  (let ((timelist (decode-time time)))
    (let ((s (car timelist))
          (m (nth 1 timelist))
          (h (nth 2 timelist)))
      (format "%02d:%02d" h (if (> s 30) (1+ m) m)))))

(defun chronos--format-seconds (seconds)
  "Format SECONDS as H:M:S, rounded to nearest second, with
  blanks replacing leading 0s and :s, and a trailing + for
  negative seconds."
  (let* ((neg (if (> 0 seconds) "+" " "))
         (seconds (abs seconds))
         (h (floor seconds 3600))
         (m (floor (- seconds (* h 3600)) 60))
         (s (round (- seconds (* h 3600) (* m 60)))))
    (cond
     ((and (zerop h)
           (zerop m)
           (zerop s)) "         ")
     ((and (zerop h)
           (zerop m)) (format "%8d%s" s neg))
     ((zerop h)       (format "%5d:%02d%s" m s neg))
     (t               (format "%2d:%02d:%02d%s" h m s neg)))))

(defun chronos--format-timer (c)
  "Format a propertized string to display timer C."
  (propertize
   (concat "["
           (chronos--time-string-rounded-to-minute (chronos--expiry-time c))
           "]    "
           (chronos--format-seconds (chronos--seconds-since-start c))
           "  "
           (chronos--format-seconds (chronos--seconds-to-expiry c))
           " "
           (chronos--message c))
   'face (cond
          ((chronos--pausedp c)  'chronos-paused)
          ((chronos--expiredp c) 'chronos-expired)
          ((chronos--nowp c)     'chronos-now)
          (t                     'chronos-default))))

;; timer notification actions

(defun chronos-modeline-notify (c)
  "Notify expiration of timer C as a message."
  (message "%s: timer %s has expired"
           (chronos--time-string-rounded-to-minute (chronos--expiry-time c))
           (chronos--message c)))

(defun chronos-buffer-notify (c)
  "Notify expiration of timer C in the notification area of the
  *chronos* buffer."
  (push (list (current-time) (chronos--message c))
        chronos--notification-list))

(defun chronos-shell-notify (c)
  "Function to run a shell command to e.g. ring bell"
  (when (stringp chronos-shell-notify-command)
    (start-process-shell-command "timer default bell"
                                 nil
                                 chronos-shell-notify-command)))

(defun chronos-dunstify (c)
  "Function to use dunstify to notify of timer C's expiry."
  (start-process-shell-command "Chronos dunstify notification"
                               nil
                               (concat "dunstify -u critical "
                                       (shell-quote-argument
                                        (chronos--time-string-rounded-to-minute
                                         (chronos--expiry-time c)))
                                       " "
                                       (shell-quote-argument
                                        (chronos--message c)))))

;; Display functions

(defun chronos--display-header ()
  "Insert header in display."
  (insert (propertize chronos-header-text 'face 'chronos-header))
  (newline))

(defun chronos--display-timers ()
  "Insert timers in display."
  (mapc (lambda (timer)
          (chronos--check-to-call-action-function timer)
          (insert (chronos--format-timer timer))
          (newline))
        chronos--timers-list))

(defun chronos--notification-expired-p (n)
  "True if notification N has expired."
  (> (float-time
      (time-subtract (current-time)
                     (car n)))
     chronos-notification-time))

(defun chronos--format-notification (n)
  "Format notification N for display"
  (concat " * " (cadr n)))

(defun chronos--display-notifications ()
  "Insert notifications in display.  Return t if any notifications are inserted, nil otherwise."
  (let ((notifications-shown nil))
    (newline 3)
    (let ((notification-start-point (point)))
      (setq fill-prefix (make-string (length chronos-notification-bullet-indent) ?\s)
            fill-column 25)
      (mapc (lambda (n)
              (unless (chronos--notification-expired-p n)
                (setq notifications-shown t)
                (let ((start (point)))
                  (insert (chronos--format-notification n))
                  (newline))))
            chronos--notification-list)
      (put-text-property notification-start-point (point) 'face 'chronos-notification)
      (fill-region notification-start-point (point)))
    notifications-shown))

(defun chronos--display-clock ()
  "Insert large current time clock in notification area of display."
  (insert (propertize (chronos--time-string-rounded-to-minute (current-time))
                        'face 'chronos-notification-clock)))

;; ensure that update-display and select-timer remain consistent.
(defun chronos--update-display ()
  "Update the list of timers displayed in chronos--buffer."
  (chronos--sort-by-expiry)
  (with-current-buffer chronos--buffer
    (let* ((inhibit-read-only t)
           (window (get-buffer-window chronos--buffer))
           (wp (window-point window)))
      (erase-buffer)
      (chronos--display-header)
      (chronos--display-timers)
      (when
          (chronos--display-notifications)
        (chronos--display-clock))
      (set-window-point window wp))))

;; ensure that update-display and select-timer remain consistent.
(defun chronos--select-timer ()
  "Return the timer shown on the cursor's line, or nil if none
  selected."
  (with-current-buffer chronos--buffer
    (let ((l (- (line-number-at-pos) 1 chronos-header-length)))
      (if (<= 0 l (1- (length chronos--timers-list)))
          (nth l chronos--timers-list)
        nil))))

(defun chronos-next-line ()
  "Move the cursor to the next usable line."
  (interactive)
  (if (>= (line-number-at-pos)
          (+ chronos-header-length
             (length chronos--timers-list)))
      (forward-line (- (length chronos--timers-list))))
  (forward-line))

(defun chronos-previous-line ()
  "Move the cursor to the next usable line."
  (interactive)
  (if (> (1- (line-number-at-pos))
         chronos-header-length)
      (forward-line -1)
    (forward-line (1- (length chronos--timers-list)))))

;; Time manipulations

(defun chronos--sort-by-expiry ()
  "Sort chronos by seconds to expiry, with longest expired and
soon to expire at the top."
  (setq chronos--timers-list (sort chronos--timers-list
                                    'chronos--expires-earlier-than-p)))

(defun chronos--start-of-day ()
  "Return a float time representing today's 00:00"
  (float-time (apply 'encode-time
                     (append '(0 0 0)
                             (nthcdr 3 (decode-time))))))

(defun chronos--parse-timestring (s &optional base)
  "Parse string S into a 4 int time list specifying an expiry
time.

An = in the string makes it an absolute time of day (technically,
relative to today 00:00), in 24+hr notation (i.e. to get 1am the
following morning, use =25:00).

Without an =, the string specifies an adjustment.  A - makes the
offset negative.  The offset is applied to BASE if specified, or
current time otherwise.

Numbers are separated by any sequence of non-digits..

One number is assumed to be minutes, two are hours/minutes and
three are hours/minutes/seconds.  Any numbers after the first
three are ignored.

There is no check of seconds/minutes to ensure that they are less
than 60, nor that hours are less than 24."
  (let ((absolutep (string-match "=" s))
        (negoffsetp (string-match "-" s))
        (sparts (mapcar 'string-to-number
                        (split-string s "[^0-9]" t))))
    (let ((b (cond
              (absolutep (chronos--start-of-day))
              (base      (float-time base))
              (t         (float-time)))))
      (let ((lp (length sparts)))
        (let ((h (if (> lp 1)
                     (car sparts)
                   0))
              (m (cond
                  ((= lp 0) 0)
                  ((= lp 1) (car sparts))
                  (t (nth 1 sparts))))
              (s (if (> lp 2)
                     (nth 2 sparts)
                   0)))
          (seconds-to-time
           (+ b
              (* (if negoffsetp -1 1)
                 (+ s (* 60 m) (* 3600 h))))))))))

;;; Main user functions - add, pause, unpause, modify expiry time, delete

;;;###autoload
(defun chronos-add-timer (time msg prefix)
  "Add a timer to expire at time TIME with message MSG.

TIME can be absolute or relative (positive countdown or negative
countup) to now or (with the prefix argument) the selected
timer."
  (interactive "sTime: \nsMessage: \nP")
  (unless chronos--buffer
    (chronos-initialize))
  (push (chronos--make-timer
         (chronos--parse-timestring
          time
          (and prefix (chronos--expiry-time (chronos--select-timer))))
         msg)
        chronos--timers-list)
  (chronos--update-display))

(defun chronos-toggle-pause-selected-line ()
  "Pause or unpause selected timer."
  (interactive)
  (chronos--toggle-pause (chronos--select-timer))
  (chronos--update-display))

(defun chronos--ensure-lap-message (c)
  "Ensures timer C has a lap style message by appending <1> if it
  does not have <n> already."
  (let ((msg (chronos--message c)))
    (unless (string-match "<[0-9]+>" msg)
      (chronos--set-message c (concat msg " <1>")))))

(defun chronos--lap-string (s)
  "Replace <n> with <n+1> in string S."
  (replace-regexp-in-string
   "<[0-9]+>"
   (lambda (m)
     (format "<%s>" (1+ (string-to-number
                         (substring m 1 -1)))))
   s))

(defun chronos-lap-selected-line ()
  "Pause the selected timer, update the message with lap
  information and start a new timer continuing the count.  The
  selected timer must be running.a"
  (interactive)
  (let ((c1 (chronos--select-timer)))
    (when (chronos--runningp c1)
      (chronos--ensure-lap-message c1)
      (let ((c1-msg (chronos--message c1))
            (c2 (chronos--copy-timer c1)))
        (chronos--pause c1)
        (chronos--set-message c1
                              (concat c1-msg
                                      (chronos--format-seconds
                                       (chronos--seconds-since-start c1))))
        (chronos--set-message c2
                              (chronos--lap-string c1-msg))
        (chronos--set-start-time c2 (current-time))
        (push c2
              chronos--timers-list)))))

(defun chronos-edit-selected-line (time prefix)
  "Adjust the expiry time of a selected running or paused timer
and optionally edit the message.

The time format is the same as for `chronos-add-timer', but the
role of the prefix key is reversed: without prefix, the
adjustment is relative to the selected timer whereas with a
prefix the adjustment is relative to the current time."
  (interactive "sTime: \nP")
  (let ((c (chronos--select-timer)))
    (when (chronos--running-or-paused-p c)
      (let ((ftime (chronos--parse-timestring time
                                              (if prefix
                                                  nil
                                                (chronos--expiry-time c))))
            (msg (read-from-minibuffer "Message: " (chronos--message c))))
        (chronos--set-expiry-time c ftime)
        (chronos--set-message c msg)
        (chronos--set-action c (not (chronos--expiredp c)))
        (chronos--update-display)))))

(defun chronos-delete-selected-line ()
  "Delete selected timer."
  (interactive)
  (let ((c (chronos--select-timer)))
    (unless (chronos--nowp c)
      (setq chronos--timers-list
            (delq c chronos--timers-list))
      (chronos--update-display))))

(defun chronos-delete-all-expired ()
  "Delete all expired timers."
  (interactive)
  (setq chronos--timers-list
        (let (tl)
          (dolist (e chronos--timers-list tl)
            (unless (chronos--expiredp e)
              (push e tl))))))

(provide 'chronos)

;;; chronos.el ends here

;;; chronos.el --- multiple simultaneous countdown / countup timers

;; Copyright (C) 2015 David Knight

;; Author: David Knight <dxknight@opmbx.org>
;; Created: 12 May 2015
;; Package-Version: 1.2
;; Version: 1.2
;; Keywords: calendar
;; URL: http://github.com/dxknight/chronos

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
;;    [17:07]           9       4:51  Coffee
;;
;; In this example, the time 'now' is 17:02. A five minute countdown
;; timer was set up 9 seconds ago.  It is expected to expire in 4 minutes
;; 51 seconds at 17:07.
;;
;; Installation
;;
;; Put this file somewhere Emacs can find it and (require 'chronos).
;;
;; `M-x chronos-add-timer' will start chronos and prompt you to add a timer.
;; When prompted for the time, enter an integer number of minutes for the timer
;; to count down from.  When prompted for the message, enter a short description
;; of the timer for display and notification.
;;
;; For more details, including more sophisticated time specifications and
;; notification options, see the info manual or website.

;;; Code:

(require 'notifications)

(defgroup chronos nil
  "Chronos' customization group."
  :group 'calendar)

(defgroup chronos-faces nil
  "Chronos' face customization subgroup."
  :group 'chronos)

(defgroup chronos-notifications nil
  "Chronos' notifications customization subgroup."
  :group 'chronos)

(defface chronos-default
  '((t (:inherit default)))
  "Basic face for chrono display."
  :group 'chronos-faces)

(defface chronos-now
  '((t (:inherit bold)))
  "Face for showing the current time."
  :group 'chronos-faces)

(defface chronos-selected
  '((t (:inherit highlight)))
  "Face for selected timer."
  :group 'chronos-faces)

(defface chronos-expired
  '((t (:inherit warning)))
  "Face for expired (counted down to zero, now counting how long ago) timers."
  :group 'chronos-faces)

(defface chronos-paused
  '((t (:inherit shadow)))
  "Face for paused timers."
  :group 'chronos-faces)

(defface chronos-header
  '((t (:inherit underline :weight bold)))
  "Face for the header line."
  :group 'chronos-faces)

(defface chronos-notification
  '((t (:inherit warning :height 8.0)))
  "Face for in-buffer notifications."
  :group 'chronos-faces)

(defface chronos-notification-clock
  '((t (:inherit bold :height 10.0)))
  "Face for in-buffer clock shown with notifications."
  :group 'chronos-faces)

(defcustom chronos-buffer-name "*chronos*"
  "Buffer name for the chronos buffer"
  :type  'string
  :group 'chronos)

(defcustom chronos-now-message "--now--"
  "Message to place on the 'now' line"
  :type  'string
  :group 'chronos)

(defcustom chronos-header-text "Expiry      Elapsed      To go  Message                  "
  "Header text for the chronos buffer"
  :type  'string
  :group 'chronos)

(defcustom chronos-expiry-functions nil
  "A hook for functions to run when a timer expires, usually to 
inform the user of its expiry.

`Chronos-expiry-functions' is an abnormal hook; all functions
must accept one argument, the expired timer."
  :type  '(repeat function)
  :group 'chronos-notifications)

(defcustom chronos-desktop-notifications-urgency 'critical
  "The urgency of expiry notifications."
  :type  '(choice (const :tag "Low" low)
                  (const :tag "Normal" normal)
                  (const :tag "Critical" critical))
  :group 'chronos-notifications)

(defcustom chronos-shell-notify-program ""
  "A shell command run when a timer expires to, for example,
  ring a bell.  Empty string is no program."
  :type  '(choice (const :tag "None" "")
                  (string :tag "Program"))
  :group 'chronos-notifications)

(defcustom chronos-shell-notify-parameters ""
  "A string or list of strings with parameters for the shell notify command"
  :type  '(choice (string)
                  (repeat string))
  :group 'chronos-notifications)

(defcustom chronos-notification-time 15
  "How many seconds to show a notification in buffer.  0 means do
not show notifications in buffer."
  :type  'integer
  :group 'chronos-notifications)

(defcustom chronos-notification-bullet-indent " * "
  "Text to use to bullet/indent notifications."
  :type  'string
  :group 'chronos-notifications)

(defcustom chronos-notification-fill-column 25
  "Column for filling notifications.  Will depend on notification
  face and window width.")

(defcustom chronos-notification-wav nil
  "Wav file to play for notification using play-sound.  Nil is no
wav used."
  :type  '(choice (const :tag "None" nil)
                  (file :tag "Wav file"))
  :group 'chronos-notifications)

(defcustom chronos-text-to-speech-program ""
  "Program to speak text for notification.  Empty string is no program."
  :type  '(choice (const :tag "None" "")
                  (file :tag "Text to speech program"))
  :group 'chronos-notifications)

(defcustom chronos-text-to-speech-program-parameters ""
  "A string or list of strings with additional parameters for
text to speech program."
  :type  '(choice (string)
                  (repeat string))
  :group 'chronos-notifications)

(defvar chronos--header-lines 1
  "How many lines in the chronos buffer header")

(defvar chronos--buffer nil
  "The special buffer for displaying timers.")

(defvar chronos--timers-list nil
  "The list of timers.")

(defvar chronos--selected-timer nil
  "The currently selected timer.")

(defvar chronos--selected-timer-point nil
  "The point at the start of the selected timer.")

(defvar chronos--notification-list nil
  "List of notifications to display in buffer notification area.")

(defvar chronos--update-timer nil
  "A run at time timer for updating the *chronos* buffer with
  chronos--update-display.")

(defvar chronos--frozenp nil
  "Whether display should be updated as normal (nil) or
  frozen (t).  With a frozen display, time continues as normal
  and will be applied - and overdue notifications made - when the
  display is unfrozen.")

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
  (add-to-list 'kill-buffer-query-functions 'chronos--kill-buffer-query)
  (add-hook 'kill-buffer-hook 'chronos--cleanup)
  (chronos-mode))

(defun chronos--kill-buffer-query ()
  "Return t if not a *chronos* buffer, or there are no timers
running or the user is ok with killing the buffer."
  (or (not (string= (buffer-name) chronos-buffer-name))
      (< (length chronos--timers-list) 2)
      (y-or-n-p "Timers are still running.  Do you really want to quit? ")))

(defun chronos--cleanup ()
  "Clean up and kill chronos."
  (when (string= (buffer-name) chronos-buffer-name)
    (when (timerp chronos--update-timer)
      (cancel-timer chronos--update-timer))
    (setq chronos--timers-list nil
          chronos--notification-list nil
          chronos--update-timer nil
          chronos--buffer nil)))

(define-key chronos-mode-map (kbd "a")      'chronos-add-timer)
(define-key chronos-mode-map (kbd "A")      'chronos-add-timers-from-string)
(define-key chronos-mode-map (kbd "SPC")    'chronos-toggle-pause-selected-line)
(define-key chronos-mode-map (kbd "e")      'chronos-edit-selected-line)
(define-key chronos-mode-map (kbd "d")      'chronos-delete-selected-line)
(define-key chronos-mode-map (kbd "l")      'chronos-lap-selected-line)
(define-key chronos-mode-map (kbd "F")      'chronos-toggle-freeze-display)
(define-key chronos-mode-map (kbd "D")      'chronos-delete-all-expired)
(define-key chronos-mode-map (kbd "n")      'chronos-next-line)
(define-key chronos-mode-map (kbd "C-n")    'chronos-next-line)
(define-key chronos-mode-map (kbd "<down>") 'chronos-next-line)
(define-key chronos-mode-map (kbd "p")      'chronos-previous-line)
(define-key chronos-mode-map (kbd "C-p")    'chronos-previous-line)
(define-key chronos-mode-map (kbd "<up>")   'chronos-previous-line)

(defun chronos--make-timer (expiry-time message &optional start)
  "Make a new timer object labled with MESSAGE that expires at
EXPIRY-TIME.  The action flag will be set to true if time to
expiry is positive, otherwise nil.

A timer is represented by a list starting with the keyword
'chronos-timer followed by TIME, MSG, ACTION, START

TIME is

* nil : for the 'now' line that represents the current time

* a float : for a paused timer, representing the seconds to go
            to (+) or past (-) expiry when it was paused.

* a 4 int list : for a running timer, representing the expiry
                 time in standard emacs time format.

MESSAGE is a string, used for labelling and notification

ACTION is a boolean, whether there should be an action on expiry
of the timer.

  when a running timer has negative time-to-go/gone and :action
  is t:

  * perform action
  * set action to nil

  when a running or paused timer has time-to-go/gone adjusted
  from -ve to +ve

  * set action to t

  when a running or paused timer has time-to-go/gone adjusted
  from +ve to -ve

  * set action to nil

START is the 4 int list creation time of the timer, or nil for
the 'now' line."
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
  "Return whether there should be an(other) action from timer C."
  (nth 3 c))

(defun chronos--set-action (c a)
  "Set whether (A = nil => no, otherwise yes) there should be
an(other) action from timer C.  Used to stop multiple action
triggers when expiry time is reached."
  (setf (nth 3 c) a))

(defun chronos--start-time (c)
  "Return start time (usually, current time when timer is
created) of timer C."
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

(defun chronos--selectedp (c)
  "Return t if C is the selected timer."
  (equal c chronos--selected-timer))

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
  "Return a float of seconds until (+ve) or since (-ve) timer
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
  (chronos--set-raw-time
   c
   (cond
    ((chronos--nowp c)     nil)
    ((chronos--runningp c) time)
    ((chronos--pausedp c)  (float-time
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

(defun chronos--time-string-rounded-to-minute (time)
  "Format TIME rounded to nearest minute."
  (let ((timelist (decode-time time)))
    (let ((s (car timelist))
          (m (nth 1 timelist))
          (h (nth 2 timelist)))
      (format "%02d:%02d" h (if (> s 30) (1+ m) m)))))

(defun chronos--time-string (c)
  "Format time rounded to nearest minute for timer c."
  (chronos--time-string-rounded-to-minute (chronos--expiry-time c)))

(defun chronos--format-seconds (seconds)
  "Format SECONDS as H:M:S, rounded to nearest second, with
  blanks replacing leading 0s and :s, and a trailing + for
  negative seconds."
  (let* ((neg (if (> 0 seconds) "+" " "))
         (seconds (abs (round seconds)))
         (h (floor seconds 3600))
         (m (floor (- seconds (* h 3600)) 60))
         (s (floor (- seconds (* h 3600) (* m 60)))))
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
          ((chronos--pausedp c)   'chronos-paused)
          ((chronos--expiredp c)  'chronos-expired)
          ((chronos--nowp c)      'chronos-now)
          ((chronos--selectedp c) 'chronos-selected)
          (t                      'chronos-default))))

(defun chronos--check-for-expiry (c)
  "Call `chronos-expiry-functions' hook if required by timer C expiring."
  (when (and chronos-expiry-functions
             (chronos--actionp c)
             (chronos--runningp c)
             (chronos--expiredp c))
    (chronos--set-action c nil)
    (run-hook-with-args 'chronos-expiry-functions c)))

(defun chronos-message-notify (c)
  "Notify expiration of timer C in the echo area and `*Messages*'
buffer."
  (message "Chronos: %s %s"
           (chronos--time-string c)
           (chronos--message c)))

(defun chronos-sound-notify (c)
  "Notify expiration of timer C by playing a wav sound file."
  (play-sound `(sound :file ,chronos-notification-wav)))

(defun chronos-desktop-notifications-notify (c)
  "Notify expiration of timer C using desktop notifications built in."
  (notifications-notify :urgency chronos-desktop-notifications-urgency
                        :title   (chronos--time-string c)
                        :body    (chronos--message c)))

(defun chronos-buffer-notify (c)
  "Notify expiration of timer C in the notification area of the
  *chronos* buffer."
  (push (list (chronos--expiry-time c)
              (chronos--message c))
        chronos--notification-list))

(defun chronos--ensure-list (p)
  "Ensure that P is a list if it is not already."
  (if (listp p) p (list p)))

(defun chronos--shell-command (name cmd parms)
  "Run shell command CMD if it exists, with parameter or
  parameter list PARMS.  Process is called NAME."
  (when (executable-find cmd)
    (start-process-shell-command name
                                 nil
                                 (combine-and-quote-strings
                                  (cons cmd
                                        (chronos--ensure-list parms))))))

(defun chronos-shell-notify (c)
  "Notify expiration of timer C by running a shell command."
  (chronos--shell-command "Chronos shell notification"
                          chronos-shell-notify-program
                          chronos-shell-notify-parameters))

(defun chronos-dunstify (c)
  "Notify expiration of timer C using dunstify."
  (chronos--shell-command "Chronos dunstify notification"
                          "dunstify"
                          (list "-u" (symbol-name chronos-desktop-notifications-urgency)
                                (chronos--time-string c)
                                (chronos--message c))))

(defun chronos-text-to-speech-notify (c)
  "Notify expiration of timer C by text-to-speech."
  (chronos--shell-command "Chronos text-to-speech notification"
                          chronos-text-to-speech-program
                          (append (chronos--ensure-list chronos-text-to-speech-program-parameters)
                                  (list (concat (chronos--time-string c)
                                                " "
                                                (chronos--message c))))))

(defun chronos--display-header ()
  "Insert header in display."
  (insert (propertize chronos-header-text 'face 'chronos-header))
  (newline))

(defun chronos--display-timers ()
  "Insert timers in display."
  (mapc #'(lambda (c)
            (chronos--check-for-expiry c)
            (when (equal c chronos--selected-timer)
              (setq chronos--selected-timer-point (point)))
            (insert (chronos--format-timer c))
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
    (let ((notification-start-point (point)))
      (newline)
      (setq fill-prefix (make-string
                         (length chronos-notification-bullet-indent)
                         ?\s)
            fill-column chronos-notification-fill-column)
      (mapc #'(lambda (n)
                (unless (chronos--notification-expired-p n)
                  (setq notifications-shown t)
                  (let ((start (point)))
                    (insert (chronos--format-notification n))
                    (newline))))
            chronos--notification-list)
      (put-text-property notification-start-point (point)
                         'face 'chronos-notification)
      (fill-region notification-start-point (point)))
    notifications-shown))

(defun chronos--display-clock ()
  "Insert large current time clock in notification area of display."
  (insert (propertize (chronos--time-string-rounded-to-minute (current-time))
                      'face 'chronos-notification-clock)))

(defun chronos-toggle-freeze-display ()
  (interactive)
  (setq chronos--frozenp (not chronos--frozenp)))

(defun chronos--position (e l &optional p)
  "Find position P of element E in list L, nil if not found."
  (let ((p (or p 0)))
    (cond
     ((null l) nil)
     ((equal e (car l)) p)
     (t (chronos--position e (cdr l) (1+ p))))))

(defun chronos--update-display ()
  "Update the list of timers displayed in the *chronos* buffer."
  (when (and (buffer-live-p chronos--buffer)
             (not chronos--frozenp))
    (chronos--sort-by-expiry)
    (with-current-buffer chronos--buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (chronos--display-header)
        (setq chronos--selected-timer-point nil)
        (chronos--display-timers)
        (when
            (chronos--display-notifications)
          (chronos--display-clock))
        (when chronos--selected-timer-point
          (goto-char chronos--selected-timer-point))))))

(defun chronos-next-line ()
  "Move the cursor to the next usable line."
  (interactive)
  (let ((next-timer (cadr (member chronos--selected-timer
                                  chronos--timers-list))))
    (setq chronos--selected-timer (or next-timer
                                      (car chronos--timers-list))))
  (chronos--update-display))

(defun chronos--previous (e l &optional r)
  "Finds the element in list L that is previous to element E.
  Returns the end of list L if E is the first element, nil or not
  found."
  (if (or (null e)
          (null l)
          (equal e (car l)))
      (or r (car (last l)))
    (chronos--previous e (cdr l) (car l))))

(defun chronos-previous-line ()
  "Move the cursor to the previous usable line."
  (interactive)
  (setq chronos--selected-timer (chronos--previous chronos--selected-timer
                                                   chronos--timers-list))
  (chronos--update-display))

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

(defun chronos--make-and-add-timer (time message base)
  "Add a timer with timespec TIME, message MESSAGE and base timer
  for relative calculations of BASE.  Returns the newly created
  timer."
  (let ((new-timer 
         (chronos--make-timer
          (chronos--parse-timestring time
                                     (chronos--expiry-time base))
          message)))
    (push new-timer chronos--timers-list)
    new-timer))

;;;###autoload
(defun chronos-add-timer (time message prefix)
  "Add a timer to expire at time TIME with message MSG.

TIME can be absolute or relative (positive countdown or negative
countup) to now or (with the prefix argument) the selected
timer."
  (interactive "sTime: \nsMessage: \nP")
  (unless chronos--buffer
    (chronos-initialize))
  (chronos--make-and-add-timer time
                               message
                               (and prefix
                                    chronos--selected-timer))
  (chronos--update-display))

(defun chronos--trim-blanks (s)
  "Trim whitespace from start/end of string S."
  (replace-regexp-in-string "\\`\\s-*\\|\\s-*\\'" "" s))

(defun chronos--split-timers-string (timers-string)
  "Split string TIMERS-STRING which may contain multiple `+' separated
cumulative timer specifications in the format <expiry spec> /
<message>.  Result is a list of (exp-spec message)"
  (mapcar #'(lambda (ts)
              (mapcar 'chronos--trim-blanks
                      (split-string ts "/")))
          (split-string timers-string "+")))

;;;###autoload
(defun chronos-add-timers-from-string (timers-string prefix)
  "Add a timer (or timers) based on TIMER-STRING.

TIMER-STRING consists of timer specifications separated by `+'s.

Timer specifications consist of an expiry specification and a
message separated by a `/'.

If the prefix argument is selected, the (first) timer will be
relative to the selected timer, otherwise current time.

Subsequent timers in the string will be relative to the previous timer.

A list of timers ((exp msg) ...) is returned."
  (interactive "sTimer specification(s): \nP")
  (unless chronos--buffer
    (chronos-initialize))
  (let ((timers (chronos--split-timers-string timers-string)))
    (let* ((previous-timer (and prefix
                                chronos--selected-timer))
           (base-timer previous-timer))
      (setq chronos--selected-timer previous-timer)
      (dolist (timer timers)
        (let ((new-timer (chronos--make-and-add-timer (car timer)
                                                      (cadr timer)
                                                      previous-timer)))
          (unless chronos--selected-timer
            (setq chronos--selected-timer new-timer))
          (setq previous-timer new-timer)))
      (chronos--update-display))
    timers))

(defun chronos-toggle-pause-selected-line ()
  "Pause or unpause selected timer."
  (interactive)
  (chronos--toggle-pause chronos--selected-timer)
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
   #'(lambda (m)
       (format "<%s>" (1+ (string-to-number
                           (substring m 1 -1)))))
   s))

(defun chronos-lap-selected-line ()
  "Pause the selected timer, update the message with lap
information and start a new timer continuing the count.  The
selected timer must be running."
  (interactive)
  (let ((c1 chronos--selected-timer))
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
        (setq chronos--selected-timer c2)
        (push c2
              chronos--timers-list)
        (chronos--update-display)))))

(defun chronos-edit-selected-line (time prefix)
  "Adjust the expiry time of a selected running or paused timer
and optionally edit the message.

The time format is the same as for `chronos-add-timer', but the
role of the prefix key is reversed: without prefix, the
adjustment is relative to the selected timer whereas with a
prefix the adjustment is relative to the current time."
  (interactive "sTime: \nP")
  (let ((c chronos--selected-timer))
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
  (let ((c chronos--selected-timer))
    (unless (chronos--nowp c)
      (chronos-next-line)
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

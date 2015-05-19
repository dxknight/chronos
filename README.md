# Chronos

Chronos provides multiple countdown / countup timers, shown sorted by expiry
time in a special buffer \*chronos\*.

    Expiry      Elapsed      To go  Message
    [17:02]                         --now--
    [17:07]          21       4:51  Coffee

Here, the time 'now' is 17:02. A five minute countdown timer was set up 21
seconds ago.  It is expected to expire in 4 minutes 51 seconds at 17:07.

    Expiry      Elapsed      To go  Message
    [17:13]          45         45+ Test run number 3
    [17:13]                         --now--

Here, a countup timer was started 45 seconds ago to time Test run number 3.

# Motivation

There are a number of Emacs packages that provide timer functionality, but I
could not find one that showed multiple simultaneous countdown and countup
timers.

Possible use cases include:

* Cooking timers (actually, what I use it most often for).
* Presentation timers (when to start wrapping up, when to start questions etc.).
* Timing how long something takes (on a scale of seconds to hours).

# Installation

Put chronos.el somewhere Emacs can find it and run (require 'chronos).  M-x
chronos-add-timer will start chronos and prompt you for an expiry time and a
message.

# Configuration

No configuration is required but you may wish to bind chronos-add-timer, set
the notification function chronos-action-function and change the faces used in
the chronos buffer.  In my init file (which uses the excellent use-package) I
set notification to be a bell played by mpv and a temporary message shown in the
buffer, and bind the add command to (C-c t):

    (use-package chronos
      :init      (progn
                  (setq chronos-shell-notify-command "mpv --really-quiet --af=scaletempo=speed=pitch --speed=0.65 ~/wip/progs/cdt/temple-bell-zen.mp3")
                  (setq chronos-action-function '(lambda (c)
                                                   (chronos-shell-notify c)
                                                   (chronos-buffer-notify c)
                                                   (chronos-dunstify c))))
      :bind      ("C-c t" . chronos-add-timer))

# Expiry time specification

The expiry time specification can be relative to current time or the expiry
of another timer, or absolute.

## absolute time

Start with an =, followed by 24hr clock.  For example, **=17:00** is an expiry
time of five o'clock in the afternoon.

## relative time

One number indicates minutes in the future, two indicates
hours and minutes and three indicates hours minutes and
seconds.  Positive numbers indicate later expiry, negative
ones earlier.

For example, if the current time is 17:00:
* 5 gives an expiry time of 17:05
* 1:30 gives 18:30
* 0:0:30 gives 30 seconds after 17:00
* 0 gives a count up timer.

Negative relative times are more useful against existing timers.  Here, a timer
was set for the absolute time 19:00, then with the cursor on this timer and
using (C-u a), two relative timers were set to expire were set for -5 (five
minutes before end, i.e. 18:55) and -15 (fifteen minutes before end, i.e. 18:45).

    Expiry      Elapsed      To go  Message 
    [18:04]                         --now--
    [18:45]          45      41:10  Any questions
    [18:55]          58      51:10  Thanks and goodbyes
    [19:00]        2:01      56:10  Talk ends

# Controls

Each timer can be paused/unpaused, adjusted or deleted.  Default keybindings
in the chronos buffer are:

* SPC - pause/unpause (pausing affects time to go and the expiry time, but not
      elapsed time)
* a   - add a timer.
* n/p - move selection down/up
* d   - delete selected timer
* e   - edit selected timer
* l   - lap selected timer
* q   - quit window
* Q   - prompt if any timers exist, kill the chronos buffer and clean up.

Whether relative times are against current time or the expiry time of the
selected timer is controlled by the prefix.

* Adding a timer with (a) is relative to current time; (C-u a) will
  calculate expiry times relative to the selected timer.

* Editing (adjusting) the selected timer with (e) will calculate times relative
  to the currently set expiry time of the timer.  (C-u e) will calculate
  relative times against the current time.

# Notifications

By default, expired timers are shown in the chronos buffer above the --now--
line until they are deleted, highlit with the chronos-expired face.  Additional
actions can be set for when a timer expires by setting `chronos-action-function'
to a custom function, perhaps referring to:

- A temporary notification in the chronos buffer, shown in a customizable
  face for a customizable period. See chronos-buffer-notify

- Running a shell command to e.g. ring a bell.  See chronos-shell-notify-command and chronos-shell-notify

- Notifying in the mode line. See chronos-modeline-notify

- Using an external notification daemon, e.g. dunstify.  See chronos-dunstify.

Countup timers (those started with 0 time to expire) do not trigger these
notifications, although they are highlit with the chronos-expired face.

If an unexpired timer is edited so that its expiry time is now in the past, no
notification will be triggered.  Conversely, adjusting an expired timer so that
its expiry time is now in the future will trigger any notifications when the
timer expires.

# Dependencies

## Emacs

Tested with GNU Emacs 24.4.1 (i686-pc-linux-gnu, X toolkit), but doesn't use
anything particularly fancy so should work on other versions.

## Not required but useful:

Some means of producing a notification sound (an example is given for MPV and
a bell mp3).

A desktop wide notification daemon (an example is given for dunstify).

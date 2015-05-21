# Chronos

Chronos provides multiple countdown / countup timers, updated every
second, shown sorted by expiry time in the special buffer \*chronos\*.

   The \*chronos\* buffer might look like:

         Expiry      Elapsed      To go  Message
         [17:02]                         --now--
         [17:07]           9       4:51  Coffee

   In this example, the time 'now' is 17:02. A five minute countdown
timer was set up 9 seconds ago.  It is expected to expire in 4 minutes
51 seconds at 17:07.

Various notification methods are available.  Here is a picture of the in-buffer
notification method, which puts the message of an expired timer in the
`chronos-notification` face for `chronos-notification-time` seconds.

![Example of in-buffer notification](example.png "In-buffer notification")

# Motivation

There are a number of Emacs packages that provide timer functionality, such as
job tracking, alarms or a countdown timer in the mode line.  However, I wanted
one that showed multiple simultaneous countdown and countup timers in a specific
buffer.

Possible use cases include:

* Cooking timers (what I use it for most often).
* Presentation timers (when to start wrapping up, when to start questions etc.).
* Timing how long something takes (on a scale of seconds to hours).

# Installation

Put chronos.el somewhere Emacs can find it and run `(require 'chronos)`.

`M-x chronos-add-timer` will start chronos and prompt you to add a timer.  When
prompted for the time, enter an integer number of minutes for the timer to count
down from (see later for more sophisticated options).  When prompted for the
message, enter a short description of the timer for display and notification.

## Dependencies

Chronos should work on a stock Emacs install.

   When a timer expires, a function `chronos-action-function' is run
with the expired timer as the argument.  If desired, this function can
be used to call external programs, for example to sound an alarm or
pop up a notification.

# Configuration

No configuration is required, but the defaults can be customized with
`M-x customize-group` chronos or set in your init file.

   You may wish to bind `chronos-add-timer`, set the notification
function `chronos-action-function` and change the faces used in the
chronos buffer.  In my init file (which uses the excellent
use-package) I have:

    (use-package chronos
      :init      (progn
                   (setq chronos-shell-notify-command "mpv --really-quiet --af=scaletempo=speed=pitch --speed=0.65 ~/wip/progs/cdt/temple-bell-zen.mp3")
                   (setq chronos-action-function '(lambda (c)
                                                    (chronos-shell-notify c)
                                                    (chronos-buffer-notify c)
                                                    (chronos-dunstify c))))
      :bind      ("C-c t" . chronos-add-timer))

This binds the `chronos-add-timer` command to `C-c t` and sets
notification to be:
* a bell sound played by mpv;
* an in-buffer temporary message shown in a large (legible from a distance) face `chronos-notification`;
* notification through dunst, a desktop notification daemon.

# Expiry time specification

The expiry time specification can be relative to current time or the expiry
of another timer, or absolute.

## absolute time

Start with an =, followed by a 24+hr clock time.  For example, **=17:00** is an
expiry time of five o'clock in the afternoon.  Times are for the current day.
If you want to refer to times tomorrow (i.e. past midnight), add 24 hours:
e.g. **=25:30** specifies 1:30 tomorrow morning.

## relative time

Up to three numbers can be given, separated with colons `:`.  A minus
`-` can be prepended to indicate negative times.

   * No numbers (or all zeros) - current time

   * One number - minutes

   * two numbers - hours and minutes and

   * three numbers - hours minutes and seconds.

Positive numbers indicate later expiry, negative ones earlier.

For example, if the current time is 17:00:
* **5** gives an expiry time of 17:05
* **1:30** gives 18:30
* **0:0:30** gives 30 seconds after 17:00
* **0** gives a count up timer starting now, at 17:00.

Negative relative times are more useful against existing timers.  Here, a timer
was set for the absolute time 19:00, then with the cursor on this timer and
using `(C-u a)`, two relative timers were set to expire earlier, one with **-5**
(five minutes before end, i.e. 18:55) and the other with **-15** (fifteen
minutes before end, i.e. 18:45).

    Expiry      Elapsed      To go  Message 
    [18:04]                         --now--
    [18:45]          45      41:10  Any questions
    [18:55]          58      51:10  Thanks and goodbyes
    [19:00]        2:01      56:10  Talk ends

# Controls

In the \*chronos\* buffer, new timers can be added, selected, paused,
unpaused, lapped, adjusted or deleted.  Default keybindings in this
buffer are:

<table>
<tr><td>a  </td><td>add a timer</td></tr>
<tr><td>n/p</td><td>move selection down/up</td></tr>
<tr><td>SPC</td><td>pause/unpause (pausing affects time to go and the expiry time, but not elapsed time)</td></tr>
<tr><td>d  </td><td>delete selected timer</td></tr>
<tr><td>D  </td><td>delete all expired timers</td></tr>
<tr><td>e  </td><td>edit selected timer</td></tr>
<tr><td>l  </td><td>lap selected timer</td></tr>
<tr><td>F  </td><td>freeze/unfreeze the display</td></tr>
<tr><td>q  </td><td>quit window</td></tr>
<tr><td>Q  </td><td>prompt if any timers exist, kill the chronos buffer and clean up</td></tr>
</table>

Whether relative times are against current time or the expiry time of the
selected timer is controlled by the prefix.

* Adding a timer with `a` is relative to current time; `(C-u a)` will
  calculate expiry times relative to the selected timer.

* Editing (adjusting) the selected timer with `e` will calculate times relative
  to the currently set expiry time of that timer.  `(C-u e)` will calculate
  relative times against the current time.

## Add a timer

Adding a timer with `a` in the \*chronos\* buffer is the same as adding
a timer with `M-x chronos-add-timer`.

## Move selection

A timer is selected when the cursor is on its line.  The cursor can be
moved down and up the timer list, wrapping as necessary, with `n` and
`p`.

   The -now- timer can be selected, but cannot be paused, lapped or
deleted.

## Pause/Unpause

`SPC` pauses a running timer and unpauses a paused one.  Pausing a
timer halts the time to/since expiry and pushes back the expected
expiry time.  The time elapsed since setting up the timer continues to
increase.  The timer is displayed in the `chronos-paused` face

## Deleting timers

The selected timer can be deleted with `d`.  Alternatively, all
expired timers can be deleted with `D`.  Once deleted, they're gone.

## Editing timers

When editing a timer, you will be prompted for an expiry time specification like
used when adding a new timer.  If the edit is started with `e`, the new expiry
time will be calculated relative to the timer's existing expiry time.  If the
edit is called with a prefix `C-u e`, the new expiry time will be relative to
the current time.

   You will also be prompted for a message, using the existing
message as a starting point.

## Lap timers

When a timer is lapped by pressing `l`:
   * The timer is paused and the time that had elapsed until the timer was
     lapped is appended to the message.  If the message does not already include
     a lap number, <1> is inserted in the message.

   * A new timer with the message <n+1> is run continuing on from the one just
     lapped.

### Lap example

   For example, a countup timer was set with Time:0 and Message:Lap
timer example.

   After running for six seconds, the timer was lapped with `l`.
Eight seconds later, the timer was lapped again, then after five
seconds and then four.  These lap times, together with lap numbers in
<> were appended to the messages.  There is no lap time for lap 5, as
that timer hasn't been lapped yet.

   The 'To go' column gives the total elapsed time, e.g. the first
four laps took 23 seconds.

   The Elapsed column gives how long since the lap started - e.g. lap
2 started 24 seconds ago.

     Expiry      Elapsed      To go  Message
     [11:50]           7         30+ Lap timer example <5>
     [11:50]          11         23+ Lap timer example <4>       4
     [11:50]          17         19+ Lap timer example <3>       5
     [11:50]          24         14+ Lap timer example <2>       8
     [11:51]          30          6+ Lap timer example <1>       6
     [11:51]                         --now--

## Freezing the display

Freezing the display with `F` prevents any updates to the \*chronos\*
buffer or triggering of expiry actions, but does not stop the timers
running.  Unfreezing the display updates the \*chronos\* buffer to
current values and triggers all outstanding actions.

## Quitting, Killing and restarting

Quitting with `q` buries the \*chronos\* buffer, but timers continue to
run.  Switch back to the \*chronos\* buffer when required.

   Killing with `Q` deletes any timers (after user confirmation) and closes the
buffer.  To restart after a kill, use `chronos-add-timer`.


# Notifications

By default, expired timers are shown above the -now- line, highlit
with the `chronos-expired` face, until they are deleted.  Additional
actions can be set for when a timer expires by setting
`chronos-action-function` to a custom function.  This function takes
a timer as an argument, which can be used to get e.g. the timer's
message.

Countup timers (those started with 0 time to expire) do not
trigger these notifications, although they are highlit with the
`chronos-expired` face.

   If an unexpired timer is edited so that its expiry time is now in
the past, no notification will be triggered.  Conversely, adjusting
an expired timer so that its expiry time is now in the future will
trigger any notifications when the timer expires.

   For examples of notification actions, see:

   * A temporary notification in the chronos buffer, shown in the
     `chronos-notification` face for `chronos-notification-time`
     seconds. See `chronos-buffer-notify`.

   * Running a shell command to e.g. ring a bell.  See
     `chronos-shell-notify-command` and `chronos-shell-notify`.

   * Notifying in the echo area / \*Messages\* buffer. See
     `chronos-message-notify`.

   * Using an external notification daemon, e.g. dunstify.  See
     `chronos-dunstify`.

   


;;; helm-chronos.el --- helm interface for standard countdowns

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

;; A helm interface to chronos.

;; helm-chronos-standard-timers should be set to a list of strings, each of
;; which has a time specifier and a message, separated with a /.  For example:
;;
;; (setq helm-chronos-standard-timers
;;       '( "     4/Tea"
;;          "=16:20/Time for tea"))
;;
;; Will offer a timer that will go off in 4 minutes from now, or a timer that
;; will expire at 4:20pm today.
;;

;;; Code:

(require 'chronos)

(defvar helm-chronos-standard-timers nil
  "A list of 'expiry time/message' strings")

(setq helm-chronos-helm-source
      '((name . "Standard timers")
        (candidates . helm-chronos-standard-timers)
        (action . (lambda (a)
                    (let* ((stl (split-string a "/"))
                           (time (car stl))
                           (message (nth 1 stl)))
                      (chronos-add-timer time message nil))))))

;;;###autoload
(defun helm-chronos-add-timer ()
  (interactive)
  (helm :sources '(helm-chronos-helm-source)))

(provide 'helm-chronos)

;;; helm-chronos.el ends here

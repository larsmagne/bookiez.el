;;; libinput.el --- Executing actions based on input events  -*- lexical-binding: t; -*-
;; Copyright (C) 2018 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: wordpress

;; Libinput is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Libinput is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defvar libinput--process nil)
(defvar libinput--prev-point nil)
(defvar libinput--device-map nil)
(defvar libinput--callback nil)
(defvar libinput--parser nil)

(defun libinput-start (callback)
  "Start listening for events."
  (libinput--start callback #'libinput--parse-line "debug-events"))

(defun libinput--start (callback parser &rest command)
  (libinput-stop)
  (with-current-buffer (get-buffer-create " *libinput*")
    (setq-local libinput--prev-point (point-max))
    (setq-local libinput--callback callback)
    (setq-local libinput--device-map (make-hash-table :test #'equal))
    (setq-local libinput--parser parser)
    (setq libinput--process
	  (apply #'start-process "libinput" (current-buffer)
		 "libinput" command))
    (set-process-filter libinput--process #'libinput--filter)
    (set-process-query-on-exit-flag libinput--process nil)))

(defun libinput-stop ()
  "Stop libinput."
  (interactive)
  (when libinput--process
    (delete-process libinput--process)))

(defun libinput--filter (process string)
  (let ((lines nil)
	callback map)
    (with-current-buffer (process-buffer process)
      (setq callback libinput--callback
	    map libinput--device-map)
      (goto-char (point-max))
      (insert string)
      (goto-char libinput--prev-point)
      (while (re-search-forward "^\\(.*\\)\n" nil t)
	(push (match-string 1) lines))
      ;; Keep the buffer trimmed.
      (save-excursion
	(forward-line -100)
	(delete-region (point) (point-min)))
      (setq libinput--prev-point (point))
      (dolist (line (nreverse lines))
	(let ((event (funcall libinput--parser line)))
	  ;; At startup (or when new devices are added), create a map
	  ;; from the physical to logical names.
	  (when event
	    (when (equal (cl-getf event :type) "DEVICE_ADDED")
	      (setf (gethash (cl-getf event :device-id) map)
		    (cl-getf event :name)))
	    (setf (cl-getf event :device-name)
		  (gethash (cl-getf event :device-id) map
			   (cl-getf event :device-id)))
	    (condition-case err
		(save-excursion
		  (funcall callback event))
	      (error
	       (message "Got an error: %s" err)))))))))

(defun libinput--parse-line (line)
  ;; The output is on the form below.  I don't know what the minus
  ;; means...
  ;; -event3   KEYBOARD_KEY     +501.91s	*** (-1) pressed
  ;;  event3   KEYBOARD_KEY     +502.02s	*** (-1) pressed
  ;; -event4   KEYBOARD_KEY     +3.719s	        KEY_MUTE (113) pressed
  ;;  event4   KEYBOARD_KEY     +3.825s	        KEY_MUTE (113) released
  ;;  event2   TOUCH_DOWN       +5.526s	        0 (0) 55.04/ 8.28 (595.00/159.00mm)

  (let* ((elem (split-string (substring line 1) "[ \n\t]+" t))
	 (type (cadr elem)))
    (append
     (list :device-id (car elem)
	   :type type)
     (cond
      ((equal type "POINTER_MOTION")
       (let ((delta (split-string (nth 3 elem) "[ /]+")))
	 (and (car delta)
	      (cadr delta)
	      (list :x-delta (string-to-number (car delta))
		    :y-delta (string-to-number (cadr delta))))))
      ((equal type "TOUCH_DOWN")
       (let ((pos (split-string (nth 5 elem) "[ /]+")))
	 (list :x (string-to-number (car pos))
	       :y (string-to-number (cadr pos)))))
      ((equal type "SWITCH_TOGGLE")
       (list :switch (nth 4 elem)
	     :state (nth 6 elem)))
      ((equal type "KEYBOARD_KEY")
       (list :key (nth 3 elem)
	     :pressed (equal (car (last elem)) "pressed")))
      ((equal type "DEVICE_ADDED")
       (list :name (mapconcat
		    #'identity
		    (cl-loop for word in (nthcdr 2 elem)
			     while (not (string-match "\\`seat" word))
			     collect word)
		    " ")))))))

(defun libinput-record (callback device-name)
  (when-let ((device (libinput--find-device device-name)))
    (libinput--start callback #'libinput--record-parser "record"
		     "--show-keycodes" device)))

(defun libinput--find-device (device-name)
  (with-temp-buffer
    (call-process "libinput" nil t nil "list-devices")
    (goto-char (point-min))
    (and (re-search-forward (format "^Device:[\t ]*%s[\t ]*$"
				    (regexp-quote device-name))
			    nil t)
	 (re-search-forward "^Kernel:[\t ]*\\(.*\\)" nil t)
	 (match-string 1))))

(defun libinput--record-parser (line)
  (and (string-match "EV_KEY / \\([^ ]+\\)[\t ]+0[\t ]*$" line)
       (list :key (match-string 1 line))))

(provide 'libinput)

;;; libinput.el ends here

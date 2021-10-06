;;; timetrack-journals.el --- Summarize clock entries of org files

;; Copyright (C) 2021  Janne Nykopp

;; Author: Janne Nykopp <newcup@iki.fi>
;; Version: 0.0.1
;; Package-requires: ((emacs "27.1"))
;; Keywords: tools
;; URL: https://peruna.fi/~newcup/timetrack-journals.el

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
;; My work flow: I write a new org mode file in a specific directory
;; every working day and clock things in that file with org-clock-in
;; (C-c C-x C-i) and org-clock-out (C-c C-x C-o).
;;
;; I want a summary of that day, both with org's `org-clock-report'
;; for listing how much time was spent on what task; and for hour
;; reporting a summary of when I started and finished at office, and
;; how much of that time was spent on work and how much on pauses, and
;; finally a difference to the basic work day length for flexi time
;; saldo.
;;
;; If there is no journal file for certain date, it means it was a day
;; off - holiday, weekend etc.
;;
;; If there's an empty journal file (or more specifically a file
;; without a `org-dblock-write:timetrack-journals-date' table), it
;; means that that day was taken off the flexi saldo and should
;; decrease the saldo by basic work days' worth.
;;
;; This file implements functions that help me to:
;;
;; 1. Easily create a daily journal file with the boilerplate
;;    placeholders or to jump to some day's file using the calendar
;;    (`timetrack-journals-days-log').
;; 
;; 2. Create the hour summary for a single day as an org-table
;;    (`org-dblock-write:timetrack-journals-date')
;;
;; 3. Easily create a monthly summary report for current month or for
;;    jumping to any month's summary using the calendar
;;    (`timetrack-journals-months-log')
;;
;; 4. Create the monthly summary report as an org-table
;;    (`org-dblock-write:timetrack-journals-month').
;;
;; Note that two new org-mode dynamic blocks are introduced. These can
;; of course be used anywhere but they might not make sense in other
;; contexts.

;; Usage: modify `timetrack-journals-journal-dir' to point to where
;; you store "journal" files. Then start a new journal with M-x
;; timetrack-journals-days-log or a specific date with C-u M-x
;; timetrack-journals-days-log. Summarize the month with M-x
;; timetrack-journals-months-log (also use prefix to choose the month
;; by choosing any day of that month).

;; Inspired by Frederik Unger's posting on org-mode mailing list
;; (https://lists.gnu.org/archive/html/emacs-orgmode/2014-11/msg00163.html)
;; but ultimately I decided to write my own to better accommodate
;; with my personal journalling practice.

;;; Code:

(provide 'timetrack-journals)

(defvar timetrack-journals-journal-dir "~/Work/journal/"
  "Where the journal files (one per day) are stored.")

(defvar timetrack-journals-workday-len (floor (* 7.5 60 60))
  "Length of a normal day at work excluding pauses. In seconds.")

;;;###autoload
(defun timetrack-journals-days-log (prefix)
  "Jump to specified day's log file. Default to today if not
called with PREFIX, otherwise present a calendar view to pick a
date. If file doesn't exist yet, insert some time tracking
boilerplate."
  (interactive "P")
  (let* ((date-str (if prefix
                       (org-read-date)
                     (format-time-string "%Y-%m-%d")))
         ;; Assume filename is always YYYY-MM-DD.org
         (filename (concat timetrack-journals-journal-dir date-str ".org")))
    (switch-to-buffer (find-file-noselect filename))
    (unless (or (file-exists-p filename) (> (buffer-size) 0))
      (let ((lines '("#+BEGIN: clocktable :scope file :maxlevel 2"
                     "#+END:"
                     ""
                     "#+BEGIN: timetrack-journals-date"
                     "#+END:")))
        (dolist (l lines)
          (insert l) (newline))))))

(defun timetrack-journals-collect-org-clock-entries ()
  "Collect all continuous (that is, two ranges of 7:00-9:00 and
9:00-11:00 are combined to 7:00-11:00) clock entries from the
current buffer. Ignore open entries (that is, no end timestamp in
org's CLOCK line). Return a list with beg-end pairs (each time as
an integer representation of the time) as conses."
  (let ((regex (concat org-clock-line-re "[[:space:]]+" org-tsr-regexp-both))
        begs ends)
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward regex nil t)
        (let* ((beg-str (match-string 1))
               (end-str (match-string 2))
               (beg-t (when beg-str
                        (time-convert (encode-time (parse-time-string beg-str)) 'integer)))
               (end-t (when end-str
                        (time-convert (encode-time (parse-time-string end-str)) 'integer))))
          (when (and beg-t end-t)
            ;; Only full ranges
            (if (cl-find beg-t ends :test #'=)
                (setf ends (cl-remove beg-t ends :test #'=))
              (push beg-t begs))
            (if (cl-find end-t begs :test #'=)
                (setf begs (cl-remove end-t begs :test #'=))
              (push end-t ends))))))
    (cl-loop for beg in (sort begs #'<)
             for end in (sort ends #'<)
             collect (cons beg end))))

(defmacro timetrack-journals--buf-to-string (&rest body)
  "Run BODY (code inserting stuff in a buffer) in a temporary
buffer and return the buffer's contents as a string."
  `(with-temp-buffer
     ,@body
     (buffer-substring-no-properties (point-min) (point-max))))

(defun timetrack-journals--insert-org-date-by-filename (&optional filename)
  "Insert org time stamp based on FILENAME or if that is not
given, the name of the file of the buffer being currently
visited."
  ;; Assume filename's date is always of format YYYY-MM-DD
  (let ((fn-date (substring (or filename (file-name-nondirectory (buffer-file-name))) 0 10)))
    (org-insert-time-stamp
     ;; Parsing just the date part gives lisp date with various
     ;; components as nil. Convert them to 0 first, otherwise
     ;; `encode-time' fails.
     (encode-time (mapcar (lambda (x) (or x 0))
                          (parse-time-string fn-date))))))

(defun timetrack-journals--lisp-time-to-hh-mm (time)
  "Convert TIME's (a list like that returned by `decode-time')
hour and minute parts to [H]H:MM format."
  (format "%d:%02d" (nth 2 time) (nth 1 time)))

(defun org-dblock-write:timetrack-journals-date (params)
  "Fill the timetrack-journals-date type org blocks with an org-table
telling date, start time, end time, elapsed worktime, time spent
at breaks and day's collected flexi time (negative or positive)."
  (declare (ignore params))
  (let* ((continuous-time-entries (timetrack-journals-collect-org-clock-entries))
         (day-start-epoch (car (cl-first continuous-time-entries)))
         (day-start (decode-time day-start-epoch))
         (day-start-hh-mm-str (timetrack-journals--lisp-time-to-hh-mm day-start))
         (day-end-epoch (cdar (last continuous-time-entries)))
         (day-end (decode-time day-end-epoch))
         (day-end-hh-mm-str (timetrack-journals--lisp-time-to-hh-mm day-end))
         (pause-sum-in-sec 0)
         pause-start pause-end)
    (when continuous-time-entries
      (insert "| ")
      (timetrack-journals--insert-org-date-by-filename)
      (while continuous-time-entries
        (setq pause-start (cdr (pop continuous-time-entries)))
        (setq pause-end (car (cl-first continuous-time-entries)))
        (when (and pause-start pause-end)
          (setq pause-sum-in-sec (+ (- pause-end pause-start) pause-sum-in-sec))))
      ;; `decode-time' called with amount of seconds that's less than
      ;; 24hrs and with timezone 0 gives us a date in 1970's but we
      ;; can just pick the hour and minute part. SO DON'T WORK ROUND
      ;; THE CLOCK!
      (let* ((pause-sum-hh-mm-str (timetrack-journals--lisp-time-to-hh-mm
                                   (decode-time pause-sum-in-sec 0)))
             (wrk-len-epoch (- day-end-epoch day-start-epoch pause-sum-in-sec))
             (wrk-len-hh-mm-str (timetrack-journals--lisp-time-to-hh-mm
                                 (decode-time wrk-len-epoch 0)))
             ;; `decode-time' trick only works with positive
             ;; times. Make negative timestrings "by hand"
             (cumulative-hh-mm-str (let* ((time (- wrk-len-epoch timetrack-journals-workday-len))
                                          (time-hh-mm (timetrack-journals--lisp-time-to-hh-mm
                                                       (decode-time (abs time) 0))))
                                     (if (< time 0)
                                         (concat "-" time-hh-mm)
                                       time-hh-mm))))
        (insert " | ") (insert day-start-hh-mm-str) (insert " | ") (insert day-end-hh-mm-str)
        (insert " | ") (insert wrk-len-hh-mm-str)
        (insert " | ") (insert pause-sum-hh-mm-str)
        (insert " | ") (insert cumulative-hh-mm-str))
      (insert " |"))))

(defun org-dblock-write:timetrack-journals-month (params)
  "Collect timetrack-journals-date rows (see
`org-dblock-write:timetrack-journals-date') for a specific
month (determined from current file's name) from each file named
YYYY-MM-DD.org under agenda directory into one main
org-table. Sum the values."
  (declare (ignore params))
  ;; Assume filename is always summary-YYYY-MM.org and individual
  ;; journals are YYYY-MM-DD.org
  (insert "| date    | start | end | work | break | flexi |\n")
  (insert "|---------+-------+-----+------+-------+-------|\n")
  (insert "| *total* |       |     |      |       |       |\n")
  (insert "|---------+-------+-----+------+-------+-------|\n")
  (let* ((yyyy-mm (substring (file-name-nondirectory (buffer-file-name)) 8 15))
         (journal-fnames (directory-files
                          timetrack-journals-journal-dir nil
                          (rx bol (eval yyyy-mm) "-" (= 2 digit) ".org" eol)))
         rows)
    (dolist (journal-fname journal-fnames)
      (with-current-buffer (find-file-noselect journal-fname)
        (save-excursion
          ;; If file is found and contains the timetrack-journals-date
          ;; table, use that data. If file is found but doesn't
          ;; contain the timetrack-journals-date table, mark that day
          ;; as having taken full day off the flexi time. If file is
          ;; not found at all, it's assumed it's a free day.
          (goto-char (point-min))
          (push (or (when (re-search-forward (rx bol "#+BEGIN:" (1+ whitespace)
                                                 "timetrack-journals-date")
                                             nil t)
                      (next-line)
                      (when (org-at-table-p)
                        ;; Could assert that org table line contains
                        ;; right amount of cells by calling
                        ;; e.g. `org-table-analyze' but maybe
                        ;; overkill for now.
                        (thing-at-point 'line t)))
                    (concat "| " (timetrack-journals--buf-to-string
                                  (timetrack-journals--insert-org-date-by-filename journal-fname))
                            ;; | start | end | work | break | flexi |
                            "  |       |     | 0:00 |  0:00 | -"
                            (timetrack-journals--lisp-time-to-hh-mm
                             (decode-time timetrack-journals-workday-len 0))
                            " |\n"))
                rows))))
    (dolist (row (reverse rows))
      (insert row))
    ;; Last row contains newline so no explicit one here. Last, apply
    ;; the formulas.
    (insert "#+TBLFM: @2$4=vsum(@II..@III);U::@2$5=vsum(@II..@III);U::@2$6=vsum(@II..@III);U")
    (next-line -2)
    (org-table-align)
    (when rows
      ;; `org-table-recalculate' gets stuck if monthly table has no
      ;;  rows (i.e. you try to create a summary of month that has no
      ;;  journal entries), hence conditional execution.
      (org-table-recalculate))))

;;;###autoload
(defun timetrack-journals-months-log (prefix)
  "Jump to specified month's summary file. Default to current
month if not called with PREFIX, otherwise present a calendar
view to pick a date (whose month is used). If file doesn't exist
yet, insert boilerplate."
  (interactive "P")
  ;; date-str: use this date's month for collecting.
  (let* ((date-str (if prefix
                       (org-read-date)
                     (format-time-string "%Y-%m-%d")))
         (yyyy-mm (substring date-str 0 7))
         (filename (concat timetrack-journals-journal-dir "summary-" yyyy-mm ".org")))
    (switch-to-buffer (find-file-noselect filename))
    (unless (or (file-exists-p filename) (> (buffer-size) 0))
      (let ((lines '("#+BEGIN: timetrack-journals-month"
                     "#+END:")))
        (dolist (l lines)
          (insert l) (newline))))
    (goto-char (point-min))
    (org-ctrl-c-ctrl-c)))

;;; timetrack-journals.el ends here

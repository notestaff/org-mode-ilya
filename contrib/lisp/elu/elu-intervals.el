;;; elu-intervals.el --- Representing sets of intervals
;; Copyright (C) 2010 Free Software Foundation, Inc.
;;
;; Author: Ilya Shlyakhter <ilya_shl at alum dot mit dot edu>
;; Keywords: time, intervals
;; Homepage: http://ilya.cc
;; Version: 0.9
;;
;; This file is not yet part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Code for representing sets of intervals.

(require 'elu)
(eval-when-compile (require 'cl))

(defstruct elu-interval
  "A time interval.  Not necessarily continuous,
i.g. could be a collection of intervals.
Abstractly, just a set of time points, and most operations
treat intervals that way.
However, currently represented as a single closed interval.

Fields:
   BEG - beginning of interval (as float number of seconds since the epoch)
   END - end of interval (as float number of seconds since the epoch)
"
  beg end)

(defun elu-interval-length (interval)
  "Return the length of the given INTERVAL."
  (elu-with 'elu-interval interval (beg end)
    (- end beg)))

(defun elu-interval-to-string (interval time-format separator)
  "Convert the interval to a standard org-mode time interval representation"
  (elu-with 'elu-interval interval (beg end)
    (mapconcat
     (apply-partially 'format-time-string time-format)
     (mapcar 'seconds-to-time (list beg end))
     separator)))


(defstruct elu-intervals
  "A set of intervals, with operations for quickly finding
intervals intersecting a given point or interval.

Currently works only for a set of same-size, contiguous, regularly spaced intervals;
but most operations make sense for an arbitrary set of intervals.

Fields:

   FROM - start of first interval
   N - number of intervals
   SHIFT - distance between starts of adjacent intervals
   WIDTH - width of each interval.
"
  from n shift width)


(defun elu-intervals-beg (intervals i)
  "Return start of I'th interval in interval set INTERVALS"
  (elu-with 'elu-intervals intervals (from shift n)
    (assert (and (integerp i) (<= 0 i) (< i n)))
    (+ from (* i shift))))

(defun elu-intervals-end (intervals i)
  "Return end of I'th interval in interval set INTERVALS"
  (+ (elu-intervals-beg intervals i) (elu-intervals-width intervals)))

(defun elu-intervals-list (intervals)
  "Return the list of intervals in INTERVALS, as elu-interval structs."
  (elu-with 'elu-intervals intervals (from n shift width)
    (let (intervals-list (beg from))
      (dotimes (i n)
	(push (make-elu-interval :beg beg :end (+ beg width)) intervals-list)
	(incf beg shift))
      (nreverse intervals-list))))

(defun elu-intervals-intersect-p (amin amax bmin bmax)
  "Test if two half-open intervals [AMIN,AMAX) and [BIN,BMAX) intersect."
  (and (< bmin amax) (< amin bmax)))

(defmacro do-elu-intervals-overlapping-interval (intervals pmin pmax i tbeg tend &rest forms)
  "Iterate over intervals in INTERVALS which intersect the interval [pmin,pmax).   Assign interval
number to I, interval start to TBEG and interval end to TEND for each interval, then execute FORMS"
  (declare (indent 6))
  (elu-with-new-symbols
   (first-interval-idx last-interval-start last-interval-end last-interval-idx from n shift width dummy)
    `(elu-with 'elu-intervals intervals (,from ,n ,shift ,width)
       (when (and (> (elu-intervals-n intervals) 0)
		  (elu-intervals-intersect-p ,pmin ,pmax ,from (elu-intervals-end ,intervals (1- ,n))))
	 (let* ((,first-interval-idx (if (and ,shift (> ,shift 0)) (floor (/ (- ,pmin ,from) ,shift)) 0))
		(,last-interval-start (+ ,from (* ,shift (1- ,n))))
		(,last-interval-end (+ ,last-interval-start ,width))
		(,last-interval-idx (if (and ,shift (> ,shift 0))
					(- ,n (floor (/ (- ,last-interval-end ,pmax) ,shift)))
				      0))
		(,tbeg (elu-intervals-beg ,intervals ,first-interval-idx))
		(,tend (+ ,tbeg ,width))
		(,i ,first-interval-idx))
	   (while (<= ,i ,last-interval-idx)
	     (when (elu-intervals-intersect-p ,pmin ,pmax ,tbeg ,tend)
	       (let ((,tbeg (max ,tbeg ,pmin))
		     (,tend (min ,tend ,pmax)))
		 ,@forms))
	     (incf ,tbeg ,width)
	     (incf ,tend ,width)
	     (incf ,i)))))))


(defmacro do-elu-intervals-containing-point (intervals p i tbeg tend &rest forms)
  "Iterate over intervals in INTERVALS which contain the point p.   Assign interval
number to I, interval start to TBEG and interval end to TEND for each interval, then execute FORMS"
  `(do-elu-intervals-overlapping-interval ,intervals ,p ,p ,i ,tbeg ,tend ,forms))

(provide 'elu-intervals)

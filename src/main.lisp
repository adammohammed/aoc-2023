(defpackage aoc23
  (:use :cl)
  (:export #'day01a))
(in-package :aoc23)

(defvar numberwords '(
		      ;; do double digits first
		      ;; since they have substrings that match the
		      ;; singledigits
		      ("eleven"    . "11")
		      ("twelve"    . "12")
		      ("thirteen"  . "13")
		      ("fourteen"  . "14")
		      ("fifteen"   . "15")
		      ("sixteen"   . "16")
		      ("seventeen" . "17")
		      ("eighteen"  . "18")
		      ("eighteen"  . "19")
		      ("nineteen"  . "20")
		      ("seven" . "7")
		      ("eight" . "8")
		      ("three" . "3")
		      ("one"   . "1")
		      ("two"   . "2")
		      ("four"  . "4")
		      ("five"  . "5")
		      ("six"   . "6")
		      ("nine"  . "9")
		      ("ten"  . "10")
		      ))

(defun day01a (input)
  "Returns sum of all calibration values."
  (let* ((lines (split-input input))
	 (calibration-values  (mapcar (lambda (x)
					(calibration-value (remove-non-digits x))) lines)))
    (reduce #'+ calibration-values)))

(defun day01b (input)
  (let* ((lines (split-input input))
	 (numbers-only (mapcar #'replace-numberwords lines))
	 (calibration-values (mapcar (lambda (x)
				       (calibration-value (remove-non-digits x))) numbers-only)))
    (reduce #'+ calibration-values)))

(defun split-input (input)
  (remove-if #'empty-string? (str:split #\Newline input)))

(defun remove-non-digits (line)
  (remove-if-not (lambda (x) (str:digitp (string x))) line))

(defun calibration-value (line)
  (let ((p1 (uiop:first-char line))
	(p2 (uiop:last-char line)))
    (multiple-value-bind (num) (parse-integer (format nil "~A~A" p1 p2)) num)))

(defun empty-string? (input)
  (string= "" input))

;; This isn't exactly right, need to look for longest match at each position
(defun replace-numberwords (line)
  (let ((result line))
    (car (last (mapcar (lambda (nw)
			 (let ((word (car nw))
			       (number (cdr nw)))
			   (setf result (str:replace-all word number result)))) numberwords)))))

(defpackage aoc23
  (:use :cl)
  (:export #'day01a))
(in-package :aoc23)

(defvar numberwords '(
		      ("zero"   . "z0o")
		      ("one"   . "o1e")
		      ("two"   . "t2o")
		      ("three" . "t3e")
		      ("four"  . "f4r")
		      ("five"  . "f5e")
		      ("six"   . "s6x")
		      ("seven" . "s7n")
		      ("eight" . "e8t")
		      ("nine"  . "n9e")
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

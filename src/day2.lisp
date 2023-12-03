(in-package :aoc23)


(defun day02a (input)
  (let ((lines (split-input input)))
    (reduce #'+ (mapcar (lambda (line)
			  (let ((id (parse-game-id line))
				(draws (parse-draws line)))
			    (if (valid-game? draws)
				id
				0))) lines))))
(defun day02b (input)
  (let ((lines (split-input input)))
    (reduce #'+ (mapcar (lambda (line)
			  (let ((draws (parse-draws line)))
			    (find-power draws))) lines))))

(defun parse-game-id (line)
  (let ((gamestr (first (str:split #\: line))))
    (multiple-value-bind (val) (parse-integer (str:substring (length "Game ") (length gamestr) gamestr))
      val)))

(defun parse-draws (line)
  (mapcar #'parse-draw (str:split #\; (second (str:split #\: line)))))

(defun parse-draw (draw)
  (mapcar #'parse-cube (str:split #\, draw)))


(defun parse-cube (cubestr)
  (let* ((parts (str:split #\Space (str:trim-left cubestr)))
	 (count  (my-parse-int (first parts)))
	 (color (second parts)))
    (cons count color)))

(defun my-parse-int (intstr)
  (multiple-value-bind (val) (parse-integer intstr)
		      val))

(defun valid-game? (draws)
  (let ((drawlength (length draws)))
    (= drawlength (length (remove-if-not #'valid-draw? draws)))))

(defun valid-draw? (draw-list)
  (let ((redmax 12)
	(greenmax 13)
	(bluemax 14))
    (all?  (mapcar (lambda (x) (let ((count (car x))
				     (color (cdr x)))
				 (cond
				   ((string= "red" color) (<= count redmax))
				   ((string= "blue" color) (<= count bluemax))
				   ((string= "green" color) (<= count greenmax))))) draw-list))))

(defun all? (xs)
  (reduce (lambda (x y) (and x y)) xs))

(defun find-power (rounds)
  (let ((maxred (max-color rounds "red"))
	(maxgreen (max-color rounds "green"))
	(maxblue (max-color rounds "blue")))
    (* maxred maxblue maxgreen)))

(defun max-color (rounds color)
  (reduce #'max
	  (flatten (mapcar (lambda (round)
			     (mapcar (lambda (p)
				       (let ((count (car p))
					     (cube-color (cdr p)))
					 (if (string= cube-color color)
					     count
					     0)))
				     round))
			   rounds))))

(defun flatten (l)
  (cond ((null l) nil)
	((atom l) (list l))
	(t (loop for n in l appending (flatten n)))))

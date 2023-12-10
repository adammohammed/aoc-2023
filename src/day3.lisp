(defun group-lines (lines)
  (if (null lines)
      nil
      (let ((prev-line *prev-line*)
	    (curr-line (car lines))
	    (next-line (cadr lines)))
	(setf *prev-line* curr-line)
	(cons (list prev-line curr-line next-line) (group-lines (cdr lines))))))

(defun find-digit-locs (line)
  (let ((matches (ppcre:all-matches "\\d+" line)))
    (loop :for (start end) :on matches :by #'cddr :while end
	  :collect (list
		    (subseq line start end)
		    (max 0 (- start 1))
		    (min (length line) (+ 1 end))))))

(defun find-adjacent-symbol (line start end)
  (if (null line)
      nil
      (> (ppcre:count-matches "[^0-9.]" line :start start :end end) 0)))


(defun find-parts (lines)
  (let ((prev-line (first lines))
	(curr-line (second lines))
	(next-line (third lines))
	(potential-matches (find-digit-locs (second lines))))
    (mapcar (lambda (match)
	      (let ((start (second match))
		    (end (third match))
		    (num (first match)))
		(cond
		  ((find-adjacent-symbol prev-line start end) num)
		  ((find-adjacent-symbol curr-line start end) num)
		  ((find-adjacent-symbol next-line start end) num)))) potential-matches)))

(defun day03a (input)
  (let ((lines (split-input input))
	(*prev-line* nil)
	parts)
    (setf parts (flatten (mapcar #'find-parts (group-lines lines))))
    (reduce #'+ (mapcar #'my-parse-int parts))))

(defun group-lines (lines)
  (if (null lines)
      nil
      (let ((prev-line *prev-line*)
	    (curr-line (car lines))
	    (next-line (cadr lines)))
	(setf *prev-line* curr-line)
	(cons (list prev-line curr-line next-line) (group-lines (cdr lines))))))

(defun group-lines-b (lines row-num)
  (if (null lines)
      nil
      (let ((prev-line *prev-line*)
	    (curr-line (car lines))
	    (next-line (cadr lines)))
	(setf *prev-line* curr-line)
	(cons (list prev-line curr-line next-line row-num) (group-lines-b (cdr lines) (incf row-num))))))


(defun find-digit-locs (line)
  (find-match-locs "\\d+" line))

(defun find-match-locs (matcher line)
  (let ((matches (ppcre:all-matches matcher line)))
    (loop :for (start end) :on matches :by #'cddr :while end
	  :collect (list
		    (subseq line start end)
		    (max 0 (- start 1))
		    (min (length line) (+ 1 end))))))

(defun find-adjacent-symbol (line start end)
  (if (null line)
      nil
      (> (ppcre:count-matches "[^0-9.]" line :start start :end end) 0)))

(defun find-gear-locs (line start end row-id)
  (if (null line)
      nil
      (let (out)
	(ppcre:do-matches
	    (s e "\\*" line out :start start :end end)
	  (push (list s e row-id) out)))))

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


(defun find-gears (lines)
  (let ((prev-line (first lines))
	(curr-line (second lines))
	(next-line (third lines))
	(row-id (fourth lines))
	(potential-matches (find-digit-locs (second lines))))
    (mapcar (lambda (match)
	      (or
	       (find-gear-candidate prev-line match (- row-id 1))
	       (find-gear-candidate curr-line match row-id)
	       (find-gear-candidate next-line match (+ row-id 1)))) potential-matches)))

(defun find-gear-candidate (line match row-id)
  (let* ((start (second match))
	(end (third match))
	(num (first match))
	(gear-locs (find-gear-locs line start end row-id)))
    (if (null gear-locs)
	nil
	(mapcar (lambda (m) (list num m)) gear-locs))))

(defun extract-gear-ratio (value)
  (if (not (= (length value) 2))
      0
      (reduce #'* value)))

(defun day03b (input)
  (let ((lines (split-input input))
	(*prev-line* nil)
	(tbl (make-hash-table :test 'equal))
	parts)
    (setf parts (flatten (mapcar #'find-gears (group-lines-b lines 0))))
    (loop :for (num start end row) :on parts :by #'cddddr :while num
	  :do
	     (let ((key (format nil "~A.~A.~A" start end row)))
	       (push (my-parse-int num) (gethash key tbl '()))))
    (reduce #'+ (loop for v being the hash-value of tbl
		      collect (extract-gear-ratio v)))))

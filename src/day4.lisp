(defun parse-day4-game (line)
  (let* ((sides (str:split "|" (cadr (str:split ":" line))))
	 (winning-nums (get-num-set (first sides)))
	 (card-nums (get-num-set (second sides))))
    (score-game (length (intersection winning-nums card-nums)))))

(defun score-game (matched-nums)
  (if (<= matched-nums 1)
      matched-nums
      (expt 2 (- matched-nums 1))))

(defun get-num-set (line)
  (let ((numbers  (remove-if #'empty-string?  (str:split " " line)))
	sset)
    (map nil (lambda (n)
	       (setf sset  (adjoin (my-parse-int n) sset))) numbers)
    sset))

(defun day04a (input)
  (let* ((lines (split-input input))
	 (results (mapcar #'parse-day4-game lines)))
     (reduce #'+ results)))

(require :asdf)
(require :uiop)

(defun nth-with-default (n list default-val)
  (if (or (< n 0)
	  (>= n (length list)))
      default-val
      (nth n list)))

(defun subseq-with-default (seq start end &key (default-val nil))
  (loop for i from start to (1- end)
	collect (nth-with-default i seq default-val)))

(defun convolve (list-1 list-2 &key (end-value 0))
  (setf list-2 (reverse list-2))
  (loop with len-2 = (length list-2)
	for start-inx = (- 1 len-2) then (1+ start-inx)
	for convolve-list = (subseq-with-default list-1
						 start-inx
						 (+ start-inx len-2)
						 :default-val end-value)
	while (< start-inx (length list-1))
	collect (loop for a in convolve-list
		      for b in list-2
		      with result = 0
		      do (setf result (+ result (* a b)))
		      finally (return result))))



(defun func (time-dist-pair)
  (loop with prod = 1
	for pair in time-dist-pair
	for time = (car pair)
	for dist = (cdr pair)
	for all-of-them = (loop for x from 0 to time collect (* x (- time x)))
	for filtered = (remove-if-not (lambda (x) (> x dist)) all-of-them)
	do (setf prod (* prod (length filtered)))
	finally (return prod)))

(defvar *actual-part-1-input* '((35 . 212)
				(93 . 2060)
				(73 . 1201)
				(66 . 1044)))

(defun part-2 (time dist)
  (loop with count = 0
	for x from 0 to time
	for val = (* x (- time x))
	do (when (> val dist)
	     (incf count))
	finally (return count)))

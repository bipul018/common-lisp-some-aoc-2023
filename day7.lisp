(require :asdf)
(require :uiop)


(defun read-file-filled-lines (filename)
  (loop for line in (uiop:read-file-lines filename)
	when (> (length line) 0)
	collect line))

(defun split-by-substring (str substr)
  (let ((substr-len (length substr))
	(str-inx 0)
	(str-len (length str)))
    (append (loop for find-inx = (search substr str :start2 str-inx)
		  while find-inx
		  collect (subseq str str-inx find-inx)
;		  do (print (subseq str str-inx find-inx))
;		  do (print find-inx)
		  do (setf str-inx (+ find-inx substr-len))
;		  do (print str-inx)
		  while (< str-inx str-len))
	    (when (< str-inx str-len)
	      (list (subseq str str-inx str-len))))))

;;Use position on this to find the value of card
(defvar *cards-for-1* '(2 3 4 5 6 7 8 9 T J Q K A))
(defvar *cards-for-2* '(J 2 3 4 5 6 7 8 9 T Q K A))


;;Given a string , insert spaces in each character and also insert brackets
(defun parse-cards (card-str)
  "This is a insecure version"
  (read-from-string (with-output-to-string (out)
		      (format out "( ")
		      (map 'nil (lambda (char) (format out "~C " char)) card-str)
		      (format out ")"))))
(defun all-equal (a b &rest others)
  (loop with init = (equal a b)
	for item in others
	do (setf init (and init (equal a item)))
	finally (return init)))

(defmacro caddddr (list-thing)
  `(car (cdr (cdddr ,list-thing))))

(defun find-hand-type (hand ref-order)
  (when (= 5 (length hand))
    (setf hand (sort (copy-list hand) #'> :key (lambda (x) (position x ref-order))))
    (let ((c1 (car hand))
	  (c2 (cadr hand))
	  (c3 (caddr hand))
	  (c4 (cadddr hand))
	  (c5 (caddddr hand)))
      (cond ((all-equal c1 c2 c3 c4 c5) 6)
	    ((or (all-equal c1 c2 c3 c4)
		 (all-equal c2 c3 c4 c5)) 5)
	    ((or (and (all-equal c1 c2)
		      (all-equal c3 c4 c5))
		 (and (all-equal c1 c2 c3)
		      (all-equal c4 c5))) 4)
	    ((or (all-equal c1 c2 c3)
		 (all-equal c2 c3 c4)
		 (all-equal c3 c4 c5)) 3)
	    ((or (and (all-equal c1 c2)
		      (all-equal c3 c4))
		 (and (all-equal c1 c2)
		      (all-equal c4 c5))
		 (and (all-equal c2 c3)
		      (all-equal c4 c5))) 2)
	    ((or (all-equal c1 c2)
		 (all-equal c2 c3)
		 (all-equal c3 c4)
		 (all-equal c4 c5)) 1)
	    (t 0)))))

(defun <-for-hands-1 (hand1 hand2)
  (if (/= (find-hand-type hand1 *cards-for-1*) (find-hand-type hand2 *cards-for-1*))
      (< (find-hand-type hand1 *cards-for-1*) (find-hand-type hand2 *cards-for-1*))
      (loop for c1 in hand1
	    for c2 in hand2
	    for v1 = (position c1 *cards-for-1*)
	    for v2 = (position c2 *cards-for-1*)
	    when (/= v1 v2) do (return (< v1 v2)))))

(defun test-part-1 (input)
  (setf input (sort (copy-list input)
		    '<-for-hands-1
		    :key (lambda (x) (car x))))
  (loop for item in input
	for rank = 1 then (1+ rank)
	with sum = 0
	do (setf sum (+ sum (* rank (cdr item))))
	finally (return sum)))
	

(defun parse-day7-line (one-line)
  (let* ((pair (split-by-substring one-line " "))
	 (hand-str (car pair))
	 (bid-str (cadr pair)))
    (cons (parse-cards hand-str) (parse-integer bid-str))))
(defun parse-day7-whole (filename)
  (loop for item in (read-file-filled-lines filename)
	collect (parse-day7-line item)))

(defun find-two-most-numerous (hand)
  (when (= 5 (length hand))
    (setf hand (sort (copy-list hand) #'> :key (lambda (x) (position x *cards-for-2*))))
    (let ((c1 (car hand))
	  (c2 (cadr hand))
	  (c3 (caddr hand))
	  (c4 (cadddr hand))
	  (c5 (caddddr hand)))
      (cond ((all-equal c1 c2 c3 c4 c5) (cons c1 nil))
	    ((all-equal c1 c2 c3 c4) (cons c1 c5))
	    ((all-equal c2 c3 c4 c5) (cons c2 c1))
	    ((all-equal c1 c2 c3) (cons c1 c5))
	    ((all-equal c2 c3 c4) (cons c2 c5))
	    ((all-equal c3 c4 c5) (cons c3 c2))
	    ((and (all-equal c1 c2)
		  (all-equal c3 c4)) (cons c4 c2))
	    ((and (all-equal c1 c2)
		  (all-equal c4 c5)) (cons c5 c2))
	    ((and (all-equal c2 c3)
		  (all-equal c4 c5)) (cons c5 c3))
	    ((all-equal c1 c2) (cons c2 c5))
	    ((all-equal c2 c3) (cons c3 c5))
	    ((all-equal c3 c4) (cons c4 c5))
	    ((all-equal c4 c5) (cons c5 c3))
	    (t (cons c5 c4))))))
    

;;fUCKING aLL bEING j
(defun transform-jokers (hand)
  (let* ((top2-pair (find-two-most-numerous hand))
	 (top1 (car top2-pair))
	 (top2 (cdr top2-pair))
	 (dst-card (if (and (equal 'J top1) top2) top2 top1)))
    (loop for c in hand
	  collect (if (equal 'J c) dst-card c))))


(defun <-for-hands-2 (hand1 hand2)
  (let ((jkr-1 (transform-jokers hand1))
	(jkr-2 (transform-jokers hand2)))
    (if (/= (find-hand-type jkr-1 *cards-for-1*)
	    (find-hand-type jkr-2 *cards-for-1*))
	(< (find-hand-type jkr-1 *cards-for-1*)
	   (find-hand-type jkr-2 *cards-for-1*))
	(loop for c1 in hand1
	      for c2 in hand2
	      for v1 = (position c1 *cards-for-2*)
	      for v2 = (position c2 *cards-for-2*)
	      when (/= v1 v2) do (return (< v1 v2))))))


(defun test-part-2 (input)
  (setf input (sort input
		    '<-for-hands-2
		    :key (lambda (x) (car x))))
  (loop for item in input
	for rank = 1 then (1+ rank)
	with sum = 0
	do (setf sum (+ sum (* rank (cdr item))))
	finally (return sum)))

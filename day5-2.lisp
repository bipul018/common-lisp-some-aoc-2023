(require :asdf)
(require :uiop)

(defun number-mapper-single (x y-init x-init len)
  "Returns nil if not in range"
  (when (and (>= x x-init)
	     (< x (+ x-init len)))
    (+ y-init (- x x-init))))

(defun number-mapper (x map-list)
  (loop for mapping in map-list
	for ymapped = (number-mapper-single x
					    (car mapping)
					    (cadr mapping)
					    (caddr mapping))
	when ymapped
	return ymapped
	finally (return x)))
(defun series-of-number-mapping (x map-series)
  (if (null map-series)
      x
      (series-of-number-mapping (number-mapper x (car map-series))
				(cdr map-series))))
(defvar *sample-part-1*)
(setf *sample-part-1* (list (list '(50 98 2)
				  '(52 50 48))
			    (list '(0 15 37)
				  '(37 52 2)
				  '(39 0 15))
			    (list '(49 53 8)
				  '(0 11 42)
				  '(42 0 7)
				  '(57 7 4))
			    (list '(88 18 7)
				  '(18 25 70))
			    (list '(45 77 23)
				  '(81 45 19)
				  '(68 64 13))
			    (list '(0 69 1)
				  '(1 0 69))
			    (list '(60 56 37)
				  '(56 93 4))))

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

(defun parse-all-integers (string)
  (reverse
  (loop with start-inx = 0
	with result = nil
	while (< start-inx (length string))
	for parse-data = (multiple-value-bind (num parsed)
			     (parse-integer string
					    :start start-inx
					    :junk-allowed t)
			   (list num parsed))
;;	do (format t "~a ~a ~a ~%" start-inx result parse-data)
	if (car parse-data)
	do (progn (setf start-inx  (cadr parse-data))
		  (push (car parse-data) result))
	else do (incf start-inx)
	finally (return result))))

(defun group-into-2 (list-of-2)
  (when list-of-2
    (cons (list (car list-of-2)
		(cadr list-of-2))
	  (group-into-2 (cddr list-of-2)))))
	  
(defun group-into-3 (list-of-3)
  (when list-of-3
    (cons (list (car list-of-3)
		(cadr list-of-3)
		(caddr list-of-3))
	  (group-into-3 (cdddr list-of-3)))))
	  

(defun parse-day5-1 (string)
  (let* ((end-str (detect-crlf-or-lf string))
	 (groups (split-by-substring string (format nil "~a~a" end-str end-str)))
	 (seed-str (car groups))
	 (mappings-str (cdr groups))
	 (seeds (parse-all-integers seed-str))
	 (mappings (loop for item in mappings-str
			 collect (group-into-3 (parse-all-integers item)))))
    (list seeds mappings)))

(defclass range-type ()
  ((start :initform 0 :accessor range.start)
   (end :initform 0 :accessor range.end)))
(defun make-range (start end)
  (when (and start end (<= start end))
    (let ((obj (make-instance 'range-type)))
      (setf (range.start obj) start
	    (range.end obj) end)
      obj)))
(defun make-range-length (start length)
  (make-range start (+ start length -1)))

(defmethod print-object ((obj range-type) out)
  (format out "[~a:~a]" (range.start obj) (range.end obj) ))
  ;; (print-unreadable-object (obj out :type t)
  ;;   (format out "[~a:~a]" (range.start obj) (range.end obj) )))

(defun num-in-range (x range)
  "Returns number if in range else nil"
  (when (and range (>= x (range.start range)) (<= x (range.end range)))
    x))

(defun split-range (range x)
  (when range
    (cons (make-range (range.start range) (min (range.end range) x))
	  (make-range (max (range.start range) x) (range.end range)))))

(defun range-left-difference (range1 range2)
  (when (and range1 range2)
    (car (split-range range1 (1- (range.start range2))))))
(defun range-right-difference (range1 range2)
  (when (and range1 range2)
    (cdr (split-range range1 (1+ (range.end range2))))))

(defun range-intersect (range1 range2)
  (when (and range1 range2)
    (car (split-range (cdr (split-range range1 (range.start range2)))
		      (range.end range2)))))


(defun sort-ranges-by-start (list-of-range)
  (sort (copy-list list-of-range) #'< :key #'range.start))

(defun sort-ranges-by-end (list-of-range)
  (sort (copy-list list-of-range) #'< :key #'range.end))

(defun reduce-range-list (list-of-range)
  (loop for sorted-range = (sort-ranges-by-start list-of-range) then (cdr sorted-range)
	for item = (car sorted-range)
	for collect-item = nil
	for process-unit = (car sorted-range)
	then (if (range-intersect process-unit item)
		 (make-range (min (range.start item) (range.start process-unit))
			     (max (range.end item) (range.end process-unit)))
		 (progn (setf collect-item process-unit)
			item))
;;	do (format t "~a & ~a & ~a~%" process-unit item collect-item)
	when collect-item
	collect collect-item
	while process-unit))

;;Now need to transform a range by another range mapping
;; Item 1 => cons of 2 lists, car is list of processed ones,
;;                            cdr is unprocessed ones, (i.e. the tail)
;; Now each step, we pop a mapping, find the right and left difference
;;   and also the intersection, then we transform the intersection acc to mapping
;;   for others we push back into unprocessed list
;; If original unprocessed list is sorted, we can maybe do it better
;;   Just keep popping it all first till we find the right difference that is not null

(defun diff-start (range1 range2)
  (- (range.start range2) (range.start range1)))

(defun map-single-one (list-of-items one-mapping)
  (loop with processed = nil
	with unprocessed = nil
	with map-range = (car one-mapping)
	with mapping = (cdr one-mapping)
	with difference = (- (range.start mapping) (range.start map-range))
	for item in list-of-items
	for right-one = (range-right-difference item map-range)
	for left-one = (range-left-difference item map-range)
	for common-one = (range-intersect item map-range)
	when common-one do (push (make-range (+ difference (range.start common-one))
					     (+ difference (range.end common-one)))
				 processed)
	when left-one do (push left-one unprocessed)
	when right-one do (push right-one unprocessed)
	finally (return (cons processed unprocessed))))

(defun apply-series-of-mappings (list-of-items mapping-list)
  (let ((unprocessed (reduce-range-list list-of-items))
	(processed nil))
;;    (format t "Input unprocessed : ~a~%" unprocessed)
    (loop for mapping in mapping-list
	  for result = (map-single-one unprocessed mapping)
;;	  do (format t "   result : ~a~%" result)
	  do (setf processed (reduce-range-list (append (car result) processed)))
	  do (setf unprocessed (cdr result)))
;;    (format t "Output unprocessed, processed : ~a, ~a~%" unprocessed processed)
    (append unprocessed processed)))

(defvar *mapping-1* )
(setf *mapping-1* (list (cons (make-range 98 99)
			      (make-range 50 51))
			(cons (make-range 50 97)
			      (make-range 52 99))))
(defvar *seeds-2*)
(setf *seeds-2* (list (make-range 79 92)
		      (make-range 55 67)))

(defun detect-crlf-or-lf (string)
  (let ((lf-pos (position #\Newline string)))
    (if (null lf-pos)
	""
	(if (or (= 0 lf-pos) (not (equal #\Return (aref string (1- lf-pos)))))
	    (format nil "~%")
	    (format nil "~C~%" #\Return)))))
	    

(defun solve-day5-2 (string)
  (let* ((end-str (detect-crlf-or-lf string))
	 (groups (split-by-substring string (format nil "~a~a" end-str end-str)))
	 (seed-str (car groups))
	 (mappings-str (cdr groups))
	 (seeds (group-into-2 (parse-all-integers seed-str)))
	 (mappings (loop for item in mappings-str
			 collect (group-into-3 (parse-all-integers item)))))
    ;;Convert each in seeds to a range
    ;;Convert each in each of mappings to a pair of ranges, source . destination
    (setf seeds (mapcar (lambda (pair) (make-range-length (car pair) (cadr pair)))
			seeds))
    (setf mappings (loop for mapping in mappings
			 collect (loop for item in mapping
				       collect (cons (make-range-length (cadr item)
									(caddr item))
						     (make-range-length (car item)
									(caddr item))))))
;;    (format t "Input list ~a~%" seeds)
    (loop for mapping in mappings
	  for output = (apply-series-of-mappings seeds mapping)
;;	  do (format t " ~a , ~a -> ~a~%" seeds mapping output)
	  do (setf seeds output))
    
;;    (format t "Solution list ~a~%" seeds)
    (apply #'min (loop for x in seeds collect (range.start x)))))


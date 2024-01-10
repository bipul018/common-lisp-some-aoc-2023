(require :asdf)
(require :uiop)

(defun range (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))

(defun cartesian-prod (join-func list1 list2)
  (mapcan (lambda (elem1)
	    (mapcar (lambda (elem2)
		      (funcall join-func elem1 elem2))
		    list2))
	  list1))
(defun filter (pass-pred list)
  (mapcan (lambda (x) (when (funcall pass-pred x) (list x)))
	  list))

(defun difference-set ( src-list sub-list &key (eq-pred #'eq))
  (filter (lambda (x) (not (find x sub-list :test eq-pred)))
	  src-list))
(defun union-set (list-1 list-2 &key (eq-pred #'eq))
  (append list-1 (difference-set list-2 list-1 :eq-pred eq-pred)))

(defclass 2d-inx ()
  ((i :accessor inx.i
      :initarg :i)
   (j :accessor inx.j
      :initarg :j)))
(defun make-2d-inx ( i j )
  (make-instance '2d-inx :i i :j j))
(defmethod print-object ((obj 2d-inx) out)
  (print-unreadable-object (obj out :type t)
    (format out "(~d, ~d)" (inx.i obj) (inx.j obj))))

(defun 2d-item (2d-inx 2d-list)
  (elt (elt 2d-list (inx.i 2d-inx))(inx.j 2d-inx) ))

(defun (setf 2d-item) (new-val 2d-inx 2d-list)
  (setf (elt (elt 2d-list (inx.i 2d-inx))(inx.j 2d-inx) ) new-val ))

(defun 2d-in-range-func (min max)
  (lambda (inx)
    (and (< (inx.i inx) (inx.i max))
	 (< (inx.j inx) (inx.j max))
	 (>= (inx.i inx) (inx.i min))
	 (>= (inx.j inx) (inx.j min)))))

(defvar *sample* (list "..9.28...."
		       "*....2..3."
		       "9...*...8."
		       "0..3...#.."))
(defun get-numbers (str)
  (let ((num-inx nil)
	(curr-inx 0)
	(res-list nil))
    (labels ((curr-num ()
	       (unless num-inx
		 (setf num-inx curr-inx)))
	     (curr-char ()
	       (when num-inx
		 (push (list num-inx
			     curr-inx
			     (read-from-string (subseq str
						       num-inx
						       curr-inx)))
		       res-list)
		 (setf num-inx nil)))
	     (process-step (ch)
	       (if (find ch "0123456789")
		   (curr-num)
		   (curr-char ))
	       (incf curr-inx)))
      (map nil #'process-step str)
      (process-step #\x)
      res-list)))

(defun process-single-num (num-list row-inx str-list)
  (let* ((inx1 (nth 0 num-list))
	 (inx2 (nth 1 num-list))
	 (num  (nth 2 num-list))
	 (row-range (range (+ 2 row-inx) :min (1- row-inx)))
	 (col-range (range (1+ inx2) :min (1- inx1)))
	 (num-inxs (cartesian-prod #'make-2d-inx (list row-inx) col-range))
	 (cart-inxs (cartesian-prod #'make-2d-inx row-range col-range)))
    ;;;(format t "~a ~a [~a:~a] ~a ~%" num-list row-inx row-range col-range cart-inxs)
    (setf cart-inxs (difference-set cart-inxs num-inxs))
    ;;;(format t "~a ~a [~a:~a] ~a ~%" num-list row-inx row-range col-range cart-inxs)
    (setf cart-inxs (filter (2d-in-range-func (make-2d-inx 0 0)
					      (make-2d-inx (length str-list)
							   (length (car str-list))))
			    cart-inxs))
    ;;;(format t "~a ~a [~a:~a] ~a ~%" num-list row-inx row-range col-range cart-inxs)
    (setf cart-inxs (filter (lambda (inx)
			      ;;;(format t "  In lambda ~a : ~a~%" inx str-list)
			      (not (find (2d-item inx str-list)
					 ".0123456789")))
			    cart-inxs))
    ;;;(format t "~a ~a [~a:~a] ~a ~%" num-list row-inx row-range col-range cart-inxs)
    (cartesian-prod #'cons
		    (mapcar (lambda (inx) (2d-item inx str-list))
			    cart-inxs)
		    (list num))))

(defun reduce-to-sum (curr-set new-elem)
  (let ((prev-elem (assoc (car new-elem) curr-set)))
;;    (format t " --Reducing : ~a with ~a ~%" curr-set new-elem)
    (if prev-elem
	(setf (cdr prev-elem) (+ (cdr prev-elem)
				 (cdr new-elem)))
	(push new-elem curr-set))
;;    (format t "  ++Got as result : ~a ~%" curr-set)
    curr-set))
	

(defun process-it (str-list)
  (let ((res nil))
    (loop for line in str-list
	  for i = 0 then (1+ i)
	  for nums = (get-numbers line)
	  do (loop for item in nums
		   do (setf res
			    (append (process-single-num item i str-list)
				    res))))
    (reduce #'reduce-to-sum res :initial-value nil)))
		      
       

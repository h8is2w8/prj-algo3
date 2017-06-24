(ql:quickload :cl-ppcre)

(defun read-capitals ()
  (with-open-file (in "../../capitals.txt")
    (loop :for line = (read-line in nil nil) :while line :collect
       (cl-ppcre:register-groups-bind (capital x y)
	   ("([A-Za-z\\s]+)\\s(\\d+)\\s(\\d+)" line)
	 (list capital (parse-integer x) (parse-integer y))))))
	   
(defun build-matrix (capitals)
  (let ((matrix (make-array (list (length capitals)
				  (length capitals)))))
    (loop :for c1 :in capitals :and i :from 0 :do
       (loop :for c2 :in capitals :and j :from 0 :do
	  (setf (aref matrix i j) (euclid-dist (cdr c1) (cdr c2)))))
    matrix))

(defun euclid-dist (x1 x2)
  (sqrt (+ (expt (- (car x1) (car x2)) 2)
	   (expt (- (cadr x1) (cadr x2)) 2))))

(defun calc-dist (path distances)
  (loop :with dist = 0
     :for i :from 1 :below (length path)
     :do (incf dist (aref distances (aref path (1- i)) (aref path i)))
     :finally (return dist)))

(defun greedy-tsp (distances &optional (s 0))
  (let ((path (make-array 0 :adjustable t :fill-pointer 0))
	(visited (make-hash-table))
	(queue (list)))

    (vector-push-extend s path)
    (setf (gethash s visited) t)
    (push s queue)

    (loop :for u = (pop queue) :while u :do
       (let ((optimal-dist most-positive-fixnum)
	     (optimal-dest -1))
	 (loop
	    :for v :from 0 :below (car (array-dimensions distances))
	    :for new-dist = (aref distances u v)
	    :do (when (and (not (gethash v visited))
			   (< new-dist optimal-dist))
		  (setf optimal-dist new-dist
			optimal-dest v)))
	 (when (> optimal-dest -1)
	   (vector-push-extend optimal-dest path)
	   (setf (gethash optimal-dest visited) t)
	   (push optimal-dest queue))))

    (vector-push-extend s path)
    path))

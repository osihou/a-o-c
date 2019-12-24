;;Day 24
(format t "Day 24~%")
(defvar *filename* 'day24/input.md)
(defvar *test1* 'day24/test1.md)

(defun open-file (filename)
  (open filename :if-does-not-exist nil))

(defun read-file (inx)
  (when inx
    (loop :for line = (read-line inx nil)
	  :while line
	  :collect line)))

(defun open-and-read (file)
  (read-file (open-file file)))

(defun get-input(filename)
  (open-and-read filename ))

(defun input->array (input)
  (make-array '(5 5) :initial-contents 
	      (loop :for i
		    :in input
		    :collect (loop :for j
				   :in (map 'list (lambda (x) x) i)
				   :collect (if (eql j #\#)
					      1
					      0)))))

(defun to-image (bn)
  (if (= bn 1)
    '\X
    '\`))

(defun print-matrix (matrix)
  (let ((dim (array-dimensions matrix)))
    (loop :for i
	  :from 0
	  :below (car dim)
	  :do (loop :for j
		    :from 0
		    :below (cadr dim)
		    :do (format t "~a " (to-image (aref matrix i j))))
	  :do (format t "~%"))))


(defun count-adj (pt matrix)
  (let ((x (car pt))
	(y (cdr pt)))
    (cond 
      ((and (= x 0) (= y 0))
       (+ (aref matrix (1+ x) y)
	  (aref matrix x (1+ y))))

      ((and (= x 4) (= y 4))
       (+ (aref matrix (1- x) y)
	  (aref matrix x (1- y))))

      ((and (= x 4) (= y 0))
       (+ (aref matrix (1- x) y)
	  (aref matrix x (1+ y))))

      ((and (= x 0) (= y 4))
       (+ (aref matrix x (1- y))
	  (aref matrix (1+ x) y)))

      ((and (> x 0) (= y 0))
       (+ (aref matrix (1+ x) y)
	  (aref matrix (1- x) y)
	  (aref matrix x (1+ y))))

      ((and (> x 0) (= y 4))
       (+ (aref matrix (1+ x) y)
	  (aref matrix (1- x) y)
	  (aref matrix x (1- y))))

      ((and (= x 0) (> y 0))
       (+ (aref matrix x (1+ y))
	  (aref matrix x (1- y))
	  (aref matrix (1+ x) y)))

      ((and (= x 4) (> y 0))
       (+ (aref matrix x (1+ y))
	  (aref matrix x (1- y))
	  (aref matrix (1- x) y)))

      (t
	(+ (aref matrix (1- x)  y)
	   (aref matrix (1+ x)  y)
	   (aref matrix  x (1- y))
	   (aref matrix  x (1+ y)))))))

(defun change-adj (matrix)
  (loop :for i
	:from 0
	:below 5
	:collect (loop :for j
		       :from 0
		       :below 5
		       :collect (count-adj (cons i j) matrix))))

(defun simulate (lst matrix)
  (make-array '(5 5) :initial-contents
	      (loop :for i
		    :in lst
		    :for j
		    :from 0
		    :below 5
		    :collect (loop :for k
				   :in i
				   :for m
				   :from 0 
				   :below 5
				   :collect (cond
					      ((and ( = (aref matrix j m) 1) (= k 1)) 
					       1)
					      ((and ( = (aref matrix j m) 0) (or (= k 1) (= k 2)))
					       1)
					      (t 
						0))))))



(defun find-true (lst)
  (loop :for i
	:in lst
	:if i
	:do (return i)
	:finally (return nil)))

(defun life-cycle-c (matrix n)
  (let ((smx matrix))
    (loop 
      :for i
      :below n
      :do (progn
	    (print-matrix smx)
	    (setf smx (simulate (change-adj smx) smx))
	    (format t "~%")))))


(defun life-cycle-u (matrix)
  (let ((smx matrix)
	(history (list matrix)))
    (loop 
      :for i
      :from 0
      :do (progn
	    (setf smx (simulate (change-adj smx) smx)))
	    (if (find-true
		  (mapcar
		    (lambda (x)
		      (if (equalp smx x)
			t
			nil))
		    history))
	      (return (cons smx i))
	      (setf history (append history (list smx))))

      :finally (return history))))


(defun biodiversity (matrix)
  (loop :for i
	:from 0
	:below 5
	:sum (loop :for j
		   :from 0
		   :below 5
		   :if (= 1 (aref matrix i j))
		   :sum (expt 2 (+ (* 5 i) j)))))

(defun day24-first ()
  (let* ((matrix (input->array (get-input *filename*)))
	 (unq (life-cycle-u matrix)))
    (format t "First: ~d~%" (biodiversity (car unq)))))


(day24-first)




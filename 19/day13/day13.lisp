(defparameter *filename* 'day13/input.md)

(defvar *lst* '())

(defun open-file (filename)
  (open filename :if-does-not-exist nil))

(defun read-file (inx)
  (when inx
    (loop for line = (read-line inx nil)
		    while line
		    collect line)))

(defun parse-string (s)
  (if s
    (if (position #\, s)
      (cons  (parse-integer (subseq s  0 (position #\, s)))
	    ( parse-string (subseq s (+ 1 (position #\, s))))))
      '()))

(defun get-input()
  (parse-string (car (read-file (open-file *filename*)))))

(defun get-memory()
  (let ((input (get-input)))
    (make-array (list (length input)) 
		:initial-contents input 
		:adjustable t)))

(defun derive-vtr (lst)
  (if (car lst)
    (cons
      (list (car lst) (cadr lst) (caddr lst))
      (derive-vtr (cdddr lst)))))


(defun mode (arg-mode num-args memory index offset)
  (when (> num-args 0)
    (multiple-value-bind (q r result) (floor arg-mode 10)
      (setq result
	    (cond
	      ((= 0 r) (aref memory index))
	      ((= 1 r) index)
	      ((= 2 r) (+ (aref memory index) offset))
	      (t (error "invalid argument"))))
      (when 
	(>= result (array-dimension memory 0))
	(adjust-array memory (list (+ 64 result)) :initial-element 0))

      (multiple-value-call #'values result
	(mode q (1- num-args) memory (1+ index) offset)))))

(defun rearange-memory (memory p n)
  (setf (nth p memory) n))

(defun intcode-machine (memory input &optional ct)

  (let 
    ((index 	(or (first ct) 0))
     (offset 	(or (second ct) 0))
     (inpt 	nil)
     (n-inpt 	input)
     (otpt 	nil)
     (a-inpt 	nil))

    (loop 
      (multiple-value-bind (arg-mode opcode) (floor (aref memory index) 100)
	(incf index)
	(cond
	  ((= opcode 1) 
	   (multiple-value-bind 
	     (arg1-index arg2-index result-index)
	     (mode arg-mode 3 memory index offset)
	     (incf index 3)
	     (setf (aref memory result-index)
		   (+ (aref memory arg1-index)
		      (aref memory arg2-index)))))

	  ((= opcode 2) 
	   (multiple-value-bind 
	     (arg1-index arg2-index result-index)
	     (mode arg-mode 3 memory index offset)
	     (incf index 3)
	     (setf (aref memory result-index)
		   (* (aref memory arg1-index)
		      (aref memory arg2-index)))))

	  ((= opcode 3)
	   (when (null n-inpt)
	     (setq awaits-input (list (1- index) offset))
	     (return))
	   (multiple-value-bind (result-index)
	     (mode arg-mode 1 memory index offset)
	     (incf index 1)
	     (setq inpt (car n-inpt) n-inpt (cdr n-inpt))
	     (setf (aref memory result-index) inpt)))

	  ((= opcode 4)
	   (multiple-value-bind (arg1-index)
	     (mode arg-mode 1 memory index offset)
	     (incf index 1)
	     (push (aref memory arg1-index) otpt)
	     (setf *lst* (append *lst* (list (aref memory arg1-index))))))

	  ((= opcode 5)
	   (multiple-value-bind (arg1-index arg2-index)
	     (mode arg-mode 2 memory index offset)
	     (if (= 0 (aref memory arg1-index))
	       (incf index 2)
	       (setq index (aref memory arg2-index)))))

	  ((= opcode 6)
	   (multiple-value-bind (arg1-index arg2-index)
	     (mode arg-mode 2 memory index offset)
	     (if (= 0 (aref memory arg1-index))
	       (setq index (aref memory arg2-index))
	       (incf index 2))))

	  ((= opcode 7)
	   (multiple-value-bind (arg1-index arg2-index result-index)
	     (mode arg-mode 3 memory index offset)
	     (incf index 3)
	     (setf (aref memory result-index)
		   (if ( < (aref memory arg1-index) 
			   (aref memory arg2-index)) 1 0))))

	   ((= opcode 8)
	    (multiple-value-bind (arg1-index arg2-index result-index)
	      (mode arg-mode 3 memory index offset)
	      (incf index 3)
	      (setf (aref memory result-index)
		    (if (= (aref memory arg1-index)
			   (aref memory arg2-index)) 1 0))))

	   ((= opcode 9)
	       (multiple-value-bind (arg1-index)
		 (mode arg-mode 1 memory index offset)
		 (incf index 1)
		 (incf offset (aref memory arg1-index))))

	   ((= opcode 99)
	    (return))
	   )))
    (values otpt a-inpt)))

(intcode-machine (get-memory) '())


(defun count-non-empty (lst)
  (length 
    (loop :for i
	  :in lst
	  :if ( = (caddr i) 2)
	  :collect i)))

(print (count-non-empty (derive-vtr *lst*)))

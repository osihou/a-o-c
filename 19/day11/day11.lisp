;iDay 11
(format t "Day 11~%")
(defvar *filename* 'day11/input.md)

(defun open-file (filename)
  (open filename :if-does-not-exist nil))


(defun read-file (inx)
  (when inx
    (loop :for line = (read-line inx nil)
	  :while line
	  :collect line)))

(defun parse-string (s)
  (if s
    (if (position #\, s)
      (cons (subseq s 0 (position #\, s))
	    (parse-string (subseq s (+ 1 (position #\, s)))))
      (cons s nil))))

(defun get-input (filename)
  (mapcar #'parse-integer 
	  (parse-string (car (read-file (open-file filename))))))

(defun get-seq (num)
  (map 'list #'digit-char-p
       (prin1-to-string num)))


(defun get-additional-memory (n)
  (loop :for i
	:from 0
	:to n
	:collect 0))

(defun create-memory (filename)
  (let* ((input (append (get-input filename) (get-additional-memory 1000)))
	 (ln (length input)))
    (make-array ln :initial-contents input )))


(defun resolve-command (seq)
  (case (length seq)
    (1 `(0		0 		0 		0 		,(car seq)) )
    (2 `(0		0		0		,(car seq) 	,(cadr seq)))
    (3 `(0		0		,(car seq) 	,(cadr seq)	,(caddr seq)))
    (4 `(0 		,(car seq)  	,(cadr seq) 	,(caddr seq)	,(cadddr seq)))
    (5 `(,(car seq)  	,(cadr seq) 	,(caddr seq) 	,(cadddr seq)	,(car (last seq))))))

(defun parse-opcode (in)
  (resolve-command
    (get-seq in)))


(defun get-A (opcode)
  (car opcode))

(defun get-B (opcode)
  (cadr opcode))

(defun get-C (opcode)
  (caddr opcode))

(defun get-D (opcode)
  (cadddr opcode))

(defun get-E(opcode)
  (car (last opcode)))

(defun set-memory (memory mode index relative n)
  (let ((current (aref memory index)))
    (case mode 
      (0 (setf (aref memory current) n))
      (2 (setf (aref memory (+ current relative)) n)))))

(defun zero-mode (memory index)
  (aref memory (aref memory index)))

(defun one-mode  (memory index)
  (aref memory index))

(defun two-mode (memory index relative)
  (aref memory (+ (aref memory index) relative)))

(defun get-mode (mode memory index relative)
  (case mode
    (0 (zero-mode memory index))
    (1 (one-mode memory index))
    (2 (two-mode memory index relative))))

(defun read-color (x y info)
  (aref info x y))

(defun turn-robot (alf cm)
  (let ((cnd (= alf 1)))
    (cond
      ((eq cm 'N)
       (if cnd
	 (setf cm 'E)
	 (setf cm 'W)))
      ((eq cm 'S)
       (if cnd
	 (setf cm 'W)
	 (setf cm 'E)))
      ((eq cm 'E)
       (if cnd
	 (setf cm 'S)
	 (setf cm 'N)))
      ((eq cm 'W)
       (if cnd
	 (setf cm 'N)
	 (setf cm 'S)))
      )))

(defun move-robot (x y cm)
  (cond 
    ((eq cm 'N) (setf y (1+ y)))
    ((eq cm 'S) (setf y (1- y)))
    ((eq cm 'E) (setf x (1+ x)))
    ((eq cm 'W) (setf x (1- x)))))
    

(defun move-if (output pst info)
  (if (and (/= (aref output 1) 999)  (/= (aref output 1) 999))
    (progn 
      (setf (aref info (car pst) (cadr pst)) (aref output 0))
      (turn-robot (aref output 1) (caddr pst))
      (move-robot pst)
      (setf (aref output 1) 999)
      (setf (aref output 0) 999)
      )))

(defun print-memory (p output x y cm info)
  (if (= output 0)
    (progn
      (setf (aref info x y) p)
      (setf output 1))

    (progn
      (turn-robot p cm)
      (move-robot x y cm)
      (setf output 0))))



(defun int-machine (memory info)
  (let ((index 0)
	(opcode 0)
	(relative 0)
	(x 75)
	(y 75)
	(cm 'N)
	(output 0))

    (loop
      (setf opcode (parse-opcode (aref memory index)))
      (cond
	((= (get-E opcode) 1)
	 (set-memory memory (get-A opcode) (+ index 3) relative 
		     (+ (get-mode (get-C opcode) memory (+ index 1) relative) 
			(get-mode (get-B opcode) memory (+ index 2) relative)))
	 (incf index 4))

	((= (get-E opcode) 2)
	 (set-memory memory  (get-A opcode) (+ index 3) relative
		     (* (get-mode (get-C opcode) memory (+ index 1) relative) 
			(get-mode (get-B opcode) memory (+ index 2) relative)))
	 (incf index 4))




	((= (get-E opcode) 3)
	 (set-memory memory (get-C opcode) (+ index 1) relative (read-color x y info))
	 (incf index 2))


	((= (get-E opcode) 4)
	 (print-memory (get-mode (get-C opcode) memory (+ index 1) relative) output x y cm info)
	 (incf index 2))






	((= (get-E opcode) 5)
	 (if (/= (get-mode (get-C opcode) memory (+ index 1) relative) 0)
	   (setf index (get-mode (get-B opcode) memory (+ index 2) relative))
	   (incf index 3)))
	 
	((= (get-E opcode) 6)
	 (if (= (get-mode (get-C opcode) memory (+ index 1) relative) 0)
	   (setf index (get-mode (get-B opcode) memory (+ index 2) relative))
	   (incf index 3)))

	((= (get-E opcode) 7)
	 (if (< (get-mode (get-C opcode) memory (+ index 1) relative)
		(get-mode (get-B opcode) memory (+ index 2) relative))
	   (set-memory memory (get-A opcode) (+ index 3) relative 1)
	   (set-memory memory (get-A opcode) (+ index 3) relative 0))
	 (incf index 4))

	((= (get-E opcode) 8)
	 (if (= (get-mode (get-C opcode) memory (+ index 1) relative)
		(get-mode (get-B opcode) memory (+ index 2) relative))
	   (set-memory memory (get-A opcode) (+ index 3) relative 1)
	   (set-memory memory (get-A opcode) (+ index 3) relative 0))
	 (incf index 4))

	((and (= (get-E opcode) 9) (= (get-D opcode) 0))
	 (setf relative (+ relative (get-mode (get-C opcode) memory (+ index 1) relative)))
	 (incf index 2))

	((and (= (get-E opcode) 9) (= (get-D opcode) 9)) 
	 (return info))))))


(defun make-canvas ()
  (make-array '(150 150) :initial-element 0))


(defun to-image (bn)
  (if (= bn 0)
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


(defun start-machine ()
  (let ((mem (create-memory *filename*))
	(canvas (make-canvas)))
    (progn
      (print-matrix (int-machine mem canvas)))))

(start-machine)


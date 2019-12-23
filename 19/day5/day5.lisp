;;;Day 5
(format t "Day 5~%")
(defvar *filename* 'day5/input.md)


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

(defun get-input ()
  (mapcar #'parse-integer (parse-string (car (read-file (open-file *filename*))))))

(defun get-seq (num)
  (map 'list #'digit-char-p
       (prin1-to-string num)))

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

(defun create-memory ()
  (let* ((input (get-input))
	 (ln (length input)))
    (make-array ln :initial-contents input)))

(defun set-memory (arr p n)
  (setf (aref arr p) n))

(defun print-memory (p)
  (format t "~%~d " p))

(defun zero-mode (memory index)
  (aref memory (aref memory index)))

(defun one-mode  (memory index)
  (aref memory index))

(defun get-mode (mode memory index)
  (case mode
    (0 (zero-mode memory index))
    (1 (one-mode memory index))))

(defun int-machine (memory)
  (let ((index 0)
	(opcode 0))
    (loop
      (setf opcode (parse-opcode (aref memory index)))
      (cond
	((= (get-E opcode) 1)
	 (set-memory memory 
		     (aref memory (+ index 3)) 
		     (+ (get-mode (get-C opcode) memory (+ index 1)) 
			(get-mode (get-B opcode) memory (+ index 2))))
	 (incf index 4))

	((= (get-E opcode) 2)
	 (set-memory memory 
		     (aref memory (+ index 3)) 
		     (* (get-mode (get-C opcode) memory (+ index 1)) 
			(get-mode (get-B opcode) memory (+ index 2))))
	 (incf index 4))

	((= (get-E opcode) 3)
	 (set-memory memory (aref memory (+ index 1)) (read))
	 (incf index 2))

	((= (get-E opcode) 4)
	 (print-memory (get-mode (get-C opcode) memory (+ index 1)))
	 (incf index 2))

	((= (get-E opcode) 5)
	 (if (/= (get-mode (get-C opcode) memory (+ index 1)) 0)
	   (setf index (get-mode (get-B opcode) memory (+ index 2)))
	   (incf index 3)))
	 
	((= (get-E opcode) 6)
	 (if (= (get-mode (get-C opcode) memory (+ index 1)) 0)
	   (setf index (get-mode (get-B opcode) memory (+ index 2)))
	   (incf index 3)))

	((= (get-E opcode) 7)
	 (if (< (get-mode (get-C opcode) memory (+ index 1))
		(get-mode (get-B opcode) memory (+ index 2)))
	   (set-memory memory (aref memory (+ index 3)) 1)
	   (set-memory memory (aref memory (+ index 3)) 0))
	 (incf index 4))

	((= (get-E opcode) 8)
	 (if (= (get-mode (get-C opcode) memory (+ index 1))
		(get-mode (get-B opcode) memory (+ index 2)))
	   (set-memory memory (aref memory (+ index 3)) 1)
	   (set-memory memory (aref memory (+ index 3)) 0))
	 (incf index 4))

	((and (= (get-E opcode) 9) (= (get-D opcode) 9)) (return opcode))))))

(defun start-machine ()
  (let ((mem (create-memory)))
      (int-machine mem)))

(defun day5-first ()
  (progn
    (format t "First: ~%")
    (format t "Please provide 1~%")
    (start-machine)))

(day5-first)

(defun day5-second ()
  (progn
    (format t "~%Second:  ~%")
    (format t "Please provide 5~%")
    (start-machine)))

(day5-second)




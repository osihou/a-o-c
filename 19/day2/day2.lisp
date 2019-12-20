;;;Day 2
(format t "Day 2~%")
(defvar *filename* 'day2/input.md)


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


(defun create-memory ()
  (let* ((input (get-input))
	 (ln (length input)))
    (make-array ln :initial-contents input)))

(defun set-memory (arr p n)
  (setf (aref arr p) n))

(defun zero-mode (memory index)
  (aref memory (aref memory index)))

(defun int-machine (memory)
  (let ((index 0)
	(opcode 0))
    (loop 
      (setf opcode (aref memory index))
      (cond
	((= opcode 1)
	 (set-memory memory (aref memory (+ index 3)) (+ (zero-mode memory (+ index 1)) (zero-mode memory (+ index 2))))
	 (incf index 4))

	((= opcode 2)
	 (set-memory memory (aref memory (+ index 3)) (* (zero-mode memory (+ index 1)) (zero-mode memory (+ index 2))))
	 (incf index 4))

	((= opcode 99) (return))))))

(defun get-output (n v)
  (let ((mem (create-memory)))
    (progn
      (set-memory mem 1 n)
      (set-memory mem 2 v)
      (int-machine mem)
      (aref mem 0))))


(defun day2-first ()
  (format t "First: ~d~%" (get-output 12 2)))

(day2-first)

(defvar *key* 19690720)

(defun get-noun-verb (key)
  (let* ((all (loop :for i
		    :from 0
		    :below 100
		    :append
		    (loop :for j
			  :from 0
			  :below 100
			  :collect (cons i j))))

	 (lst (loop :for i
		    :in all
		    :collect (get-output (first i) (rest i)))))

    (nth  (- (length (member key (reverse lst))) 1) all)))


(defun day2-second ()
  (let ((answer (get-noun-verb *key*)))
  (format t "Second: ~d~%" (+ (* 100 (first answer)) (rest answer)))))

(day2-second)

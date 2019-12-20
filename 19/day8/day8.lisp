;;Day 8
(format t "Day 8~%")
(defvar *filename* 'day8/input.md)

(defun open-file (filename)
  (open filename :if-does-not-exist nil))

(defun read-file (inx)
  (when inx
    (loop :for line = (read-line inx nil)
	  :while line
	  :collect line)))

(defun open-and-read (file)
  (car (read-file (open-file file))))

  
(defun get-seq (num)
  (map 'list #'digit-char-p 
       (prin1-to-string num)))

(defun cdr-machine (num lst)
  (if (/= 0 num)
    (cdr-machine (1- num) (cdr lst))
    lst))

(defun get-layer (input n)
  (loop :for i 
	:in input
	:for j 
	:below n
	:collect i))

(defun get-layers (input n)
  (if (car input)
    (cons (get-layer input n)
	  (get-layers (cdr-machine n input) n))))

(defun sum-of (input n)
  (length 
    (loop :for i 
	  :in input
	  :if  (= i n)
	  :collect i)))

(defun sum-of-zeros (input)
  (sum-of input 0))
	
(defun get-input()
  (get-seq (parse-integer (open-and-read *filename* ))))

(defun find-lowest (input)
  (let ((x (loop :for i
		 :in input
		 :minimize i
		 :into m
		 :finally (return m))))
    (loop :for i
	  :in input
	  :for j
	  :from 0
	  :when (= i x)
	  :return j)))
	  


(defun get-least-zero-layer ()
  (let ((x (get-layers (get-input) 150) ))
    (nth 
      (find-lowest
	(mapcar #'sum-of-zeros x))
      x)))

(defun one-two (lst)
  (* (sum-of lst 1) (sum-of lst 2)))

(defun day8-first ()
  (format t "First: ~d~%" (one-two (get-least-zero-layer))))

(day8-first)


(defun reduct (i j)
  (if (/= i 2)
    i
    j))

(defun compare (main sub)
  (loop :for i
	:in main
	:for j
	:in sub
	:collect (reduct i j)))


(defun value-input (main sub)
  (if (car sub)
    (value-input (compare main (car sub)) (cdr sub))
  main))

(defun decode-message (input)
  (value-input (car input) (cdr input)))

(defun to-image (bn)
  (if (= bn 1)
    '\X
    '\`))


(defun print-image (lst)
  (if (car lst)
    (progn
      (format t "~{ ~a ~}~%" (mapcar #'to-image (car lst)))
      (print-image (cdr lst)))))

(defun day8-second ()
  (progn
    (format t "Second:~%")
    (print-image (get-layers 
		   (decode-message (get-layers (get-input) 150)) 25))))


(day8-second)



;;Day 8


(defun open-file (filename)
  "Open a file"
  (open filename :if-does-not-exist nil))

(defun close-file (inx)
  "close file"
  (close inx))

(defun read-file (inx)
  "read file"
  (when inx
    (loop for line = (read-line inx nil)
		    while line
		    collect line)))

(defun parse-string (s)
  "parse input"
  (if s
    (if (position #\, s)
      (cons  (parse-integer (subseq s  0 (position #\, s)))
	    ( parse-string (subseq s (+ 1 (position #\, s))))))
      '()))

(defun sorted (lst predicate) 
  "is list sorted"
  (apply predicate lst))
   
(defun get-seq (num)
  "get sequance of numbers"
  (map 'list #'digit-char-p 
       (prin1-to-string num)))

(defun cdr-machine (num lst)
  "returns lst after num cdrs"
  (if (/= 0 num)
    (cdr-machine (1- num) (cdr lst))
    lst))


(defun get-layer (input n)
  (loop for i in input
	for j below n
	collect i))

(defun get-layers (input n)
  (if (car input)
    (cons (get-layer input n)
	  (get-layers (cdr-machine n input) n))))

(defun open-and-read (file)
  (car (read-file
	 (open-file 'day8/input.md))))

(defun sum-of (input n)
  (length 
    (loop :for i 
	  :in input
	  :if  (= i n)
	  :collect i)))

(defun sum-of-zeros (input)
  (sum-of input 0))
	
(defun get-input()
  (get-seq (parse-integer (open-and-read 'day8/input.md ))))

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

(print (one-two (get-least-zero-layer)))

; 0 - black
; 1 - white
; 2 - transparent


(defun reduct (i j)
  (if (/= i 2)
    i
    j))

(defun compare (main sub)
  (loop :for i
	:in main
	:for j
	:in sub
	collect (reduct i j)))


(defun value-input (main sub)
  (if (car sub)
    (value-input (compare main (car sub)) (cdr sub))
  main))

(defun decode-message (input)
  (value-input (car input) (cdr input)))

(defun print-image (lst)
  (if (car lst)
    (progn
      (print (car lst))
      (print-image (cdr lst)))))


(print-image (get-layers (decode-message (get-layers (get-input) 150)) 25))

































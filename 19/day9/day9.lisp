;;Day 7


(defun all-permutations (list)
  "all posible permutations"
  (cond ((null list) nil)
	((null (cdr list)) (list list))
	 (t (loop for element in list
		  append (mapcar (lambda (x) (cons element x))
				 (all-permutations (remove element list)))))))


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
  (if (/= 0 num)
    (cdr-machine (1- num) (cdr lst))
    lst))

(defun ckr (lst)
  (car (cddr (cddr lst))))

;;***MACHINE***

(defvar *relative-base* 0 )

(defun pos-or-imm (seq stack program)
  (case  seq 
    (0 (nth  stack program))
    (1 stack)
    (2 (nth (+ *relative-base* stack) program)))) 

(defun add (program stack seq)
  "opcode 01 adds together numbers read from two positions and stores the
  resoult in the third position XXX01 A B C"
  (progn
    (case (car seq) 
	     (0 (setf (nth (caddr stack) program) 
		      (+ (pos-or-imm (caddr seq) (car stack) program)
		      (pos-or-imm (cadr seq) (cadr stack) program))))
	     (2 (setf (nth (+ *relative-base* (caddr stack)) program) 
		      (+ (pos-or-imm (caddr seq) (car stack) program)
		      (pos-or-imm (cadr seq) (cadr stack) program))))) 
	
    (int-machine program (cdddr stack))))

(defun mult (program stack seq)
  "opcode 02 multiplies the two inputs from two positions and stores the
  resoult in the third position XXX02 A B C"
  (progn
    (case (car seq) 
	     (0 (setf (nth (caddr stack) program) 
		      (* (pos-or-imm (caddr seq) (car stack) program)
		      (pos-or-imm (cadr seq) (cadr stack) program))))
	     (2 (setf (nth (+ *relative-base* (caddr stack)) program) 
		      (* (pos-or-imm (caddr seq) (car stack) program)
		      (pos-or-imm (cadr seq) (cadr stack) program))))) 
	    (int-machine program (cdddr stack))))

(defun store (program stack seq)
  "opcode 03 takes a single integer as input and saves it to the position 
  given by its only parameter"
  (progn
    (print 'INPUT)
	   (case (caddr seq) 
	     (0 (setf (nth (car stack) program) (read)))
	     (2 (setf (nth (+ *relative-base* (car stack)) program) (read))))
  (int-machine program (cdr stack))))

(defun output (program stack seq)
  "opcode 4 outputs the value of its only parameter"
  (progn
    (print (pos-or-imm (caddr seq) (car stack) program))
    (int-machine program (cdr stack))))

(defun jump-if-true (program stack seq)
  "opcode 05 is jump-if-true, if the first argument is non-zero, it sets the instruction
  pointer to the value from the second parameter"
  (if (/= (pos-or-imm (caddr seq) (car stack) program) 0)
    (int-machine program (cdr-machine (pos-or-imm (cadr seq) (cadr stack) program) program))
    (int-machine program (cddr stack))))

(defun jump-if-false (program stack seq)
  "opcode 06 is jump-if-false, if the first parameter is zero, it sets the instruction 
  pointer to the value from the second parameter"
  (if (= (pos-or-imm (caddr seq) (car stack) program) 0)
    (int-machine program (cdr-machine  (pos-or-imm (cadr seq) (cadr stack) program) program))
    (int-machine program (cddr stack))))


(defun less-than (program stack seq)
  "opcode 07 is less-than, if the first parameter is less than the second 
  parameter it stores 1 in the position given by third parameter, otherwise it 
  stores 0"
  (progn
    (if (< (pos-or-imm (caddr seq) (car stack) program) (pos-or-imm (cadr seq) (cadr stack) program))
	(case (car seq) 
	     (0 (setf (nth (caddr stack) program) 1))
	     (2 (setf (nth (+ *relative-base* (caddr stack)) program) 1)))
	
      	(case (car seq) 
	     (0 (setf (nth (caddr stack) program) 0))
	     (2 (setf (nth (+ *relative-base* (caddr stack)) program) 0))))

    (int-machine program (cdddr stack))))
 
(defun equals (program stack seq)
  "opcode 08 is equals, if the first parameter is equalt to the second 
  parameter, it stores 1 in the position given by the third parameter,
  otherwise it stores 0"
  (progn
    (if (= (pos-or-imm (caddr seq) (car stack) program) (pos-or-imm (cadr seq) (cadr stack) program))
      	(case (car seq) 
	     (0 (setf (nth (caddr stack) program) 1))
	     (2 (setf (nth (+ *relative-base* (caddr stack)) program) 1)))
      	(case (car seq) 
	     (0 (setf (nth (caddr stack) program) 0))
	     (2 (setf (nth (+ *relative-base* (caddr stack)) program) 0))))

    (int-machine program (cdddr stack))))


(defun adjust-base (program stack seq)
  "the 09 opcode to adjust the base"
  (progn
    (setf *relative-base* 
	  (+ *relative-base* 
	     (pos-or-imm (caddr seq) (car stack) program)))
    (int-machine program (cdr stack))))
   

(defun execute-command (program seq stack)
  (case (ckr seq)
    (1 (add program stack seq))
    (2 (mult program stack seq))
    (3 (store program stack seq))
    (4 (output program stack seq))
    (5 (jump-if-true program stack seq))
    (6 (jump-if-false program stack seq))
    (7 (less-than program stack seq))
    (8 (equals program stack seq))
    (9 (adjust-base program stack seq))))

;; ABCDE
; 	C - mode of the first parameter
; 	B - mode of the second parameter
;	A - mode of the third parameter
;
;	0 - position mode
;	1 - immediate-mode
;	2 - relative-mode

(defun gen-memory ()
  (loop for i
	from 0
	below 1000
	collect 0))

(defun resolve-command ( program seq stack)
  (case (length seq)
    (1 (execute-command program `( 0 		0 		0 		0 	,(car seq)) stack))
    (3 (execute-command program `( 0 		0 		,(car seq) 	0 	,(caddr seq)) stack))
    (4 (execute-command program `( 0  		,(car seq) 	,(cadr seq) 	0 	,(cadddr seq)) stack))
    (5 (execute-command program `( ,(car seq) 	,(cadr seq) 	,(caddr seq) 	0 	,(ckr seq)) stack))))


(defun int-machine (program stack )
  "recursive int machine"
;  (progn 
;    (print stack)
;    (print program)
;    (print *relative-base*)
;    (read)
  (if (= (car stack) 99 )
    nil
    (if (car stack)
      (resolve-command program 
		       (get-seq (car stack)) (cdr stack)))))
;)
		

(defun get-program (filename)
  (parse-string (car (read-file (open-file filename)))))


(defvar in (open-file 'day9/input.md))
(defvar *programe*  (append (parse-string (car (read-file in))) (gen-memory)))
;(print *programe*)
;(print (length *programe*))
(int-machine *programe* *programe*)

;(print *programe*)
;***TESTS***
(print '***TESTS***)

(defun test-program (lst)
  (let ((test lst))
    (progn
      (int-machine lst test)
      (print lst))))

;(test-program  (append '(109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99) (gen-memory)))

;(test-program  (append '(1102 34915192 34915192 7 4 7 99 0) (gen-memory)))

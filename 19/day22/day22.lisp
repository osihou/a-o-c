;;;Day 22 
(format t "Day 22~%")
(defvar *filename* 'day22/input.md )

(defun flatten (lst &optional stack out)
  (cond ((consp lst) (flatten (rest lst) (cons (first lst) stack) out))
	(lst (flatten (first stack ) (rest stack) (cons lst out)))
	(stack (flatten (first stack) (rest stack) out))
	(t out)))


(defun open-file (filename)
  (open filename :if-does-not-exist nil))

(defun read-file (inx)
  (when inx
    (loop :for line = (read-line inx nil)
	  :while line
	  :collect line)))


(defun remove-spaces (lst)
  (if (position #\space lst)
    (subseq lst 0 (position #\space lst))
    lst))


(defun parse-string (s)
  (if s
    (if (position #\space s)
      (cons  (subseq s  0 (position #\space s))
	    ( parse-string (subseq s (+ 1 (position #\space s)))))
      (list s))))

(defun add (x y)
  (+ x y))

(defun add-two (x)
  (+ x  2))

(defun str->fname (lst)
  (intern 
    (if (> (length lst) 1)
      (format nil "~{~:@(~a~)~#[~;-~:;-~]~}" lst)
      (format nil "~:@(~a~)" (car lst)))))


(defun parse-command (str)
  (let* ((raw (parse-string str))
	 (num (parse-integer (car (last raw)) :junk-allowed t)))
    (cons (str->fname (set-difference raw (last raw))) num)))


(defun evaluate (command lst)
  (progn
    (print command)
  (eval `(,(car command) '(,lst) ,(cdr command)))))


(defun get-input ()
  (read-file (open-file *filename*)))


(defun get-deck ()
  (loop :for i
	:from 0
	:to 10006
	:collect i))

(defun deal-into-new (lst &optional junk)
  (reverse (car lst)))

(print (evaluate '(deal-into-new . NIL) '(1 2 3)))

(defun cut (lst n)
  (progn 
    (print lst)
  (let ((reminder
	  (if ( > n 0)
	    (nthcdr n (car lst))
	    (nthcdr (+ (length (car lst)) n) (car lst)))))
    (append reminder (set-difference (car lst) reminder)))))


(defun deal-with-increment (lst n)
  (progn 
    (print (length (car lst)))
  (let* ((plst  (car lst))
	 (setx (loop :for i
		     :from 0
		     :below n 
		     :collect (reverse
				(loop :for j
				      :in (cdr plst)
				      :for k
				      :from 0
				      :when (= (mod k n) i)
				      :collect j)))))
      (print (cons (car plst) (flatten setx))))))

(print (nthcdr 2 '(1 2 3)))
;(print (get-input))

(defun shuffle (lst commands)
  (loop :for i
	:in commands
	:do (setf lst (evaluate (parse-command i) lst))
	:finally (return lst)))

(print (shuffle (get-deck) (get-input)))

(defun day22-first ())

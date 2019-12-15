;;Day 14

(defparameter *filename* 'day14/input.md)

(defun open-file (filename)
  (open filename :if-does-not-exist nil))

(defun read-file (inx)
  (when inx
    (loop for line = (read-line inx nil)
		    while line
		    collect line)))

(defun parse-string (s)
  (if s
    (if (position #\space s)
      (cons  (subseq s  0 (position #\space s))
	    ( parse-string (subseq s (+ 1 (position #\space s))))))
      '()))

(defun get-input()
  (read-file (open-file *filename*)))

(defun remove-commas (lst)
  (if (position #\, lst)
    (subseq lst 0 (position #\, lst))
    lst))

(defun parse-strings (lst)
  (if (car lst)
    (cons  (mapcar #'remove-commas (parse-string (car lst)))
	    (parse-strings (cdr lst)))))

(print (parse-strings (get-input)))


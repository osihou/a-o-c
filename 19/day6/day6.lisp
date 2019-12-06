;; Day 6

;;***FIRST***

(defun open-file (filename)
  "Open a file"
  (open filename :if-does-not-exist nil))

(defun close-file (inx)
  "close file"
  (close inx))

(defun parse-string (s)
  (if s
    (if (position #\) s)
      (cons  (intern ( subseq s  0 (position #\) s)) ) 
	    (intern ( subseq s (1+ (position #\) s))))))
      '()))



(defun read-file (inx)
  "read file"
  (when inx
    (loop for line = (read-line inx nil)
		    while line
		    collect line)))


(defvar *input* (read-file (open-file 'day6/input.md)))

(defvar *test* (read-file (open-file 'day6/test.md)))

(defun count-orbits (mp start)
  (loop :for i := (gethash start mp nil)
	:then (gethash i mp nil)
	:while i
	:count t))
	
(defun split (seq denom)
  (labels
    ((rec (start accum)
	  (let ((index (position denom seq :start start)))
	    (if index
	      (rec (+ 1 index ) (cons (subseq seq start index) accum))
	      (cons (subseq seq start) accum)))))
    (nreverse (rec 0 nil))))

(defun set-map (input)
  (loop 
    :for i in *input*
    :with mp := (make-hash-table)
    (v k)  := (mapcar 'read-from-string (split i #\) ))
    :do (setf (gethash k  mp) v )
    :finally (return mp)))

(print (set-map  *input* ))

(format t "t: ~a"(loop 
	 :for start 
	 :being :the :hash-values :of *mp*
	 :sum (count-orbits *mp* start)))







(in-package #:lol)

;;; flatten
;;; flatten is not work in sbcl
;;; for ,x is not '(, x) is a
;;; struct not a list?
(defun flatten (tree)
  (labels ((rec (tree result)
	     (cond ((consp tree)
		    (append (rec (car tree) result)
			    (rec (cdr tree) result)))
		   #+sbcl
		   ((sb-impl::comma-p tree)
		    (rec (sb-impl::comma-expr tree) result))
		   (t  (list tree)))))
    (remove '() (rec tree '()))))

(defun symb (&rest args)
  (values (intern 
	   (with-output-to-string (s)
	     (dolist (x args)
	       (princ x s))))))


(defun group (lists n)
  (if (zerop n) (error "n is zero, invalid."))
  (if (> (length lists) n)
      (cons (subseq lists 0 n)
	    (group (nthcdr n lists) n))
      (list lists)))




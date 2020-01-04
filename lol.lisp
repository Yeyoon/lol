;;;; lol.lisp

(in-package #:lol)

;;; This is the dlambda implemented
;;; I just do not know whether it is
;;; has free variable capture issue.
(defmacro dlambda (&rest ds)
  (let ((as (gensym "as")))
    `(lambda (&rest ,as)
       (case (car ,as)
	 ,@(loop for d in ds collect
		 `(,(car d)
		   (apply (lambda ,(second d) ,(third d))
			  ,(if (eq t (car d))
			      `,as
			      `(cdr ,as)))))))))



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
		    (list (sb-impl::comma-expr tree)))
		   (t  (list tree)))))
    (remove '() (rec tree '()))))


;;; chapter 3
;;;  sleep-uints%
;;;
(defun sleep-units% (value unit)
  (sleep
   (* value
      (case unit
	((s) 1)
	((m) 60)
	((h) 3600)
	((d) 86400)
	((ms) 1/1000)
	((us) 1/1000000)))))


(defmacro unit-of-time (value unit)
  `(* ,value
      ,(case unit
	((s) 1)
	((m) 60)
	((h) 3600)
	((d) 86400)
	((ms) 1/1000)
	((us) 1/1000000))))

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

;;; chapter 5
;;; defuints
;;;
(defmacro defunits (quantity base-unit &rest units)
  (let ((value (gensym "value"))
	(unit (gensym "unit")))
    `(defmacro ,(symb 'unit-of- quantity) (,value ,unit)
       `(* ,,value
	   ,(case ,unit
	      ((,`,base-unit) 1)
	      ,@(expand-units `,base-unit (group `,units 2))))))) 


#|
(defunits time 
  s
  m (1/60 h)
  h (60 m))
|#

(defun get-unit-chain (u units)
  (car (member u units :test #'eq :key #'car)))

(defun chain-unit (u base-unit units)
  (if (eq base-unit u)
      1
      (* (caadr (get-unit-chain u units))
	 (chain-unit (cadadr (get-unit-chain u units)) base-unit units))))

(defun expand-unit (unit base-unit &rest units)
  (let ((lu (group units 2)))
    (chain-unit unit base-unit lu)))


(defun expand-units (base-unit units)
  (loop :for u :in units :collect
	`((,(car u)) ,(chain-unit (car u) base-unit units))))
       
  
  

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
		    (rec (sb-impl::comma-expr tree) result))
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


#||
(defunits time 
  s
  m (1/60 h)
  h (60 m))
||#

(defun get-unit-chain (u units)
  (car (member u units :test #'eq :key #'car)))

(defun depend-unit-pair (upair base-unit)
  (if (consp (second upair))
      upair
      `(,(car upair) (,(second upair) ,base-unit))))

(defun get-u-depend (u base-unit units)
  (second
   (depend-unit-pair (get-unit-chain u units)
		     base-unit)))

(defun chain-unit (u base-unit units history)
  (if (member u history)
      (error "~a depends on ~a depends on ~a~%" u (cadadr (get-unit-chain u units)) u))
  (if (eq base-unit u)
      1
      (let ((upair (get-u-depend u base-unit units)))
	(* (first upair)
	   (chain-unit (second upair) base-unit units (push u history))))))

(defun expand-units (base-unit units)
  (loop :for u :in units :collect
	`((,(car u)) ,(chain-unit (car u) base-unit units '()))))


#||
nlet: usage
(defun nlet-fact (n)
  (nlet fact ((n n))
    (if (zerop n)
      1
      (* n (fact (- n 1))))))

---->
(defun nlet-fact (n)
  (let ((n n))
    (labels ((fact (n)
	       (if (zerop n)
		   1
		   (* n (fact (- n 1))))))
      (fact n))))
||#
(defmacro nlet (name letargs &rest body)
  `(labels ((,name ,(mapcar #'first letargs)
	      ,@body))
     (,name ,@(mapcar #'second letargs))))


#||
for defun defmacro/g!
useage:
(defmacro/g! nif (expr pos zero neg)
  `(let ((,g!result ,expr))
     (cond ((plusp ,g!result) ,pos)
           ((zerop ,g!result) ,zero)
            (t ,neg))))
||#
(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s) "G!" :start1 0 :end1 2)))

#||
what defmacro/g! does is automate using gensym 
to create symbol :
(defmacro/g! nif (expr pos zero neg)
  `(let ((,g!result ,expr))
     (cond ((plusp ,g!result) ,pos)
           ((zerop ,g!result) ,zero)
            (t ,neg))))

--->
(defmacro nif (expr pos zero neg)
  (let ((g!result (gensym "result")))
    `(let ((,g!result ,expr))
       (cond ((plusp ,g!result) ,pos)
	     ((zerop ,g!result) ,zero)
	     (t ,neg)))))

so to create this macro template as follows:
1. search the g!xx symbol
2. create gensym using these symbol
3. add the defmacro 
||#

(defmacro defmacro/g! (name args &rest body)
  ;; 1. search the g!xx symbols
  (let ((gsyms (remove-duplicates
		(remove-if-not #'g!-symbol-p (flatten body)))))
    `(defmacro ,name ,args
       (let ,(loop :for g :in gsyms :collect `(,g (gensym ,(subseq (symbol-name g) 2))))
	 ,@body))))


#||
what defmacro! does is add a 'let over lambda' 
over defmacro/g! 
usage:
(defmacro! nif (o!expr pos zero neg)
  `(cond ((plusp ,g!expr) ,pos)
         ((zerop ,g!expr) ,zero)
          (t ,neg)))
||#
(defun o!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s) "O!" :start1 0 :end1 2)))

(defun o!-symbol-to-g!-symbol (s)
  (symb "G!" (subseq (symbol-name s) 2)))

(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
	 (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(loop :for g :in (list ,@gs)
		    :for o :in (list ,@os)
		    :collect (append (list g) (list o)))
	  ,,@body))))


#||

(defun build-legargs (gs os)
  (loop :for g :in gs :for o :in os :collect
				    (list g o)))

(defun build-f (gs os)
  (mapcar #'list gs os))

*** This macro does not work
*** As when defmacro/g! expand
*** which is before the g!xx symbol
*** That will cause o!x not 
*** used error
(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
	 (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(build-legargs ,gs ,os)
	  ,,@body))))


***
***  This is the let over lambda definition
***  As I currently understand that, using
***  list function here is to expand the gs 
***  os things at the defmacro! expand and 
***  the list function will called the args
***  eval after the defmacro/g! expand, at 
***  that point the g!x is gensymed (eval
***  time).  
***
(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))

***  This is the same as the first defmacro!
***  when build-legargs eval in the defmacro/o!
***  expand. Which expands to ((g!x o!x)), this
***  will not be expanded in the defmacro/g! 
***  expand
***
(defmacro defmacro/o! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(build-legargs ',gs ',os)
          ,(progn ,@body)))))

***  This is the same as the above
(defmacro defmacro/o!! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(funcall #'build-f ',gs ',os)
          ,(progn ,@body)))))

||#


#||
for this example
(defmacro! sq (o!x) 
  `(* ,g!x ,g!x))

---->
(defmacro sq (o!x)
  (let ((g!x (gemsym "x")))
    `(let ((,g!x ,o!x))
       (* ,g!x ,g!x))))

As `(let ((,g!x ,o!x))
      (* ,g!x ,g!x))

---->
(list 'let `((,g!x ,o!x))
      `(* ,g!x ,g!x))
---->
(list 'let (list `(,g!x ,o!x))
      (list '* g!x g!x))
---->
(list 'let (list (list g!x o!x))
      (list '* g!x g!x))

||#

  

#||
chapter 4 Read Macro
for #> to read strings.
||#
(defun |#>-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  ;; 1. first to read the delimita words
  (let ((chars))
    (do ((ch (read-char stream) (read-char stream)))
	((char= #\newline ch))
      (push ch chars))

    ;; 2. continue to read until match
    ;; chars stop. and which will be
    ;; the contents
    (let ((keys (nreverse chars))
	  (outputs))
      (do ((ch (read-char stream) (read-char stream)))
	  ((null keys) outputs)
	(push ch outputs)
	(if (char= ch (car keys))
	    (setf keys (cdr keys))
	    (setf keys chars))
	(if (null keys)
	    (return)))
      (coerce
       (reverse (nthcdr (length chars) outputs))
       'string))))

(set-dispatch-macro-character #\# #\> #'|#>-reader|)

  
	    

#||
segment-reader
usage:
(segment-reader t #\/ 3)
abc/def/ghi/

("abc" "def" "ghi")
||#
(defun segment-reader (stream ch numberarg)
  (if (> numberarg 0)
      (let ((chars))
	(do ((c (read-char stream) (read-char stream)))
	    ((char= c ch)
	     (cons
	      (coerce (nreverse chars) 'string)
	      (segment-reader stream ch (- numberarg 1))))
	  (push c chars)))))
	  

	
      

;;#+cl-ppcre
(defun |#~-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((mode-char (read-char stream)))
    (cond
      ((char= mode-char #\m)
         (match-mode-ppcre-lambda-form
           (segment-reader stream
                           (read-char stream)
                           1)))
      ((char= mode-char #\s)
         (subst-mode-ppcre-lambda-form
           (segment-reader stream
                           (read-char stream)
                           2)))
      (t (error "Unknown #~~ mode character")))))

;;#+cl-ppcre
(set-dispatch-macro-character #\# #\~ #'|#~-reader|)

#||
Original ppcre mode is :
#+cl-ppcre
(defmacro! match-mode-ppcre-lambda-form (o!args)
 ``(lambda (,',g!str)
     (cl-ppcre:scan
       ,(car ,g!args)
       ,',g!str)))

#+cl-ppcre
(defmacro! subst-mode-ppcre-lambda-form (o!args)
 ``(lambda (,',g!str)
     (cl-ppcre:regex-replace-all
       ,(car ,g!args)
       ,',g!str
       ,(cadr ,g!args))))

now rewrite it without the defamcro!
||#


#||
Add some expanations here:
defmacro! will expandto 
(let ((g!args (gensym "args"))
      (g!str (gensym "str")))
  `(let ((,g!args ,o!args)) ;; here is why
     `(lambda (,',g!str)      ;; defmacro! has
	(cl-ppcre-scan      ;; implemented
	 ,(car ,g!args)     ;; once-only
	 ,',g!str))))       ;; it eval once here

||#
;;; using gensym
(defmacro match-mode-ppcre-lambda-form (args)
  (let ((x (gensym))
	(y (gensym)))
    `(let ((,y ,args))
       `(lambda (,',x)
	  (cl-ppcre:scan
	   ,(car ,y)
	   ,',x)))))


(defmacro subst-mode-ppcre-lambda-form (args)
  (let ((x (gensym))
	(y (gensym)))
    `(let ((,y ,args))
       `(lambda (,',x)
	  (cl-ppcre:regex-replace-all
	   ,(car ,y)
	   ,',x
	   ,(cadr ,y))))))



(defun atest (n)
  (macrolet ((fact (n acc)
	       `(progn
		  (psetq ,@(apply #'nconc
				  (mapcar #'list '(n acc)
					  (list n acc))))
		  (go bxx))))
    (block cxx
      (let ((n n) (acc 1))
	(tagbody
	 bxx
	   (return-from cxx
	     (progn
	       (if (zerop n)
		   acc
		   (fact (- n 1) (* acc n))))))))))


(defmacro nlet-tail% (n letargs &rest body)
  (let ((gs (loop :for x :in letargs :collect (gensym))))
    `(macrolet ((,n ,gs
		  `(progn
		     (psetq 
		      ,@(loop :for x :in ',letargs
			      :for y :in (list ,@gs)
			      :append
			      `(,(car x) ,y)))
		      (go abcdefg))))
       (block ab
	 (let ,letargs
	   (tagbody
	    abcdefg
	      (return-from ab
		(progn
		  ,@body))))))))


;; using defmacro! rewrite it
(defmacro! nlet-tail (n letargs &rest body)
  (let ((gs (loop :for x :in letargs :collect (gensym))))
    `(macrolet ((,n ,gs
		  `(progn
		     (psetq
		      ,@(loop :for x :in ',letargs
			   :for y :in (list ,@gs)
			   :append
			   `(,(car x) ,y)))
		     (go ,',g!tag))))
       (block ,g!block
	 (let ,letargs
	   (tagbody
	      ,g!tag
	      (return-from ,g!block
		(progn
		  ,@body))))))))

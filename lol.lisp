;;;; lol.lisp

(in-package #:lol)



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


;; using nlet-tail
(defmacro! cxr (x tree)
  (if (null x)
      tree
      (let ((op (cond
		  ((eq 'a (cadr x)) 'car)
		  ((eq 'd (cadr x)) 'cdr)
		  (t (error "Non A/D symbol")))))
	`(nlet-tail
	  ,g!name ((,g!count ,(car x))
		   (,g!val (cxr ,(cddr x) ,tree)))
	  (if (>= 0 ,g!count)
	      ,g!val
	      (,g!name (- ,g!count 1)
		       (,op ,g!val)))))))


(defmacro def-english-list-accessors (start end)
  (if (not (<= 1 start end))
      (error "Bad start/end range")
      `(progn
	 ,@(loop :for i :from start :to end :collect
	      `(defun ,(symb
			(map 'string
			     (lambda (c)
			       (if (alpha-char-p c)
				   (char-upcase c)
				   #\-))
			     (format nil "~:r" i)))
		   (arg)
		 (cxr (1 a ,(- i 1) d) arg))))))


(defun cxr-symbol-p (sym)
  (if (symbolp sym)
      (let ((chars (coerce (symbol-name sym) 'list)))
	(and (< 6 (length chars))
	     (char= #\C (car chars))
	     (char= #\R (car (last chars)))
	     (null (remove-if (lambda (c)
				(or (char= #\A c)
				    (char= #\D c)))
			      (cdr (butlast chars))))))))


(defun cxr-symbol-to-cxr-list (cxrsym)
  (if (cxr-symbol-p cxrsym)
      (let ((cxrchars (cdr ;; chop c
		       (butlast ;; chop r
			(coerce (symbol-name cxrsym) 'list)))))
	(loop :for c :in cxrchars :append
	   `(1 ,(if (char= c #\A)
		    'a
		    'd))))))


(defmacro with-all-cxrs (&rest forms)
  (let ((cxrsymbol (remove-duplicates
		    (remove-if-not
		     #'cxr-symbol-p
		     (flatten forms)))))
    `(labels (,@(loop :for cs :in cxrsymbol :collect
		   `(,cs (l)
			 (cxr ,(cxr-symbol-to-cxr-list cs) l))))
       ,@forms)))






(defmacro alet-fsm (&rest states)
  `(macrolet ((state (s) `(setq this #',s)))
     (labels (,@states)
       #',(caar states))))


#||
ichain-before
(alet ((acc 0))
    (ichain-before
      (format t "Changing from ~a~%" acc))
    (lambda (n)
      (incf acc n)))
||#
(defmacro! ichain-before (&rest body)
  `(let ((,g!env this))
     (setq this
	   (lambda (&rest ,g!arg)
	     ,@body
	     (apply ,g!env ,g!arg)))))


(defmacro! ichain-after (&rest body)
  `(let ((,g!env this))
     (setq this
	   (lambda (&rest ,g!arg)
	     (prog1
		 (apply ,g!env ,g!arg)
	       ,@body)))))

(defmacro! ichain-intercept (&rest body)
  `(let ((,g!env this))
     (setq this
	   (lambda (&rest ,g!arg)
	     (block ,g!intercept
	       (macrolet ((intercept (v)
			    `(return-from ,',g!intercept ,v)))
		 (prog1
		     (apply ,g!env ,g!arg)
		   ,@body)))))))

(defmacro alet-hotpatch% (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest args)
       (if (eq (car args) :hotpatch)
	   (setq this (second args))
	   (apply this args)))))

(defmacro alet-hotpatch (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (dlambda
      (:hotpatch (clourse) (setq this (second clourse)))
      (t (apply this args)))))

;; closing
(defmacro! let-hotpatch (letargs &rest body)
  `(let ((,g!this) ,@letargs)
     (setq ,g!this ,@(last body))
     ,@(butlast body)
     (dlambda
      (:hotpatch (,g!clouser) (setq ,g!this ,g!clouser))
      (t (&rest ,g!args) (apply ,g!this ,g!args)))))



;; Let-binding-transform
(defun let-binding-transform (bs)
  (if bs
      (loop :for x :in bs :collect
			  (cond ((symbolp x) (list x))
				((consp x) x)
				(t (error "Not support type ~a~%" x))))))

;; sub-let
(defmacro sub-let (binding% &rest body)
  (let ((bindings
	  (mapcar (lambda (x)
		    (cons (gensym (symbol-name (car x)))
			  x))
		  (let-binding-transform binding%))))
    `(let (,@(loop :for x :in bindings :collect
		   `(,(first x) ,(third x))))
       ,@(tree-leaves
	  body
	  #1=(member x bindings :key #'second)
	  (caar #1#)))))



(defmacro sub-let* (bindings &rest body)
  `(sub-let ,bindings
	    ,@(mapcar #'macroexpand-1 body)))






;;; make stats counter
(defun make-stats-counter
    (&key (count 0)
       (sum 0)
       (sum-of-squares 0))
  (plambda (n) (sum count sum-of-squares)
	   (incf sum-of-squares (expt n 2))
	   (incf sum n)
	   (incf count)))

(defmacro defpan (name args &rest body)
  `(defun ,name (self)
     ,(if args
	  `(with-pandoric ,args self
			  ,@body)
	  `(progn ,@body))))


(defpan stats-counter-mean (sum count)
  (/ sum count))

(defpan stats-counter-variance
    (sum-of-squares sum count)
  (if (< count 2)
      0
      (/ (- sum-of-squares
	    (* sum
	       (stats-counter-mean self)))
	 (- count 1))))

(defpan stats-counter-stddev ()
  (sqrt (stats-counter-variance self)))

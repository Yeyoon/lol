(in-package #:lol)

(defmacro alet (letargs &rest body)
  `(let ((this) ,@letargs)
     (setf this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

(defmacro alambda (params &rest body)
  `(labels ((self ,params ,@body))
     #'self))

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


(defun tree-leaves%% (tree test result)
  (if tree
      (if (listp tree)
	  (cons (tree-leaves%% (car tree) test result)
		(tree-leaves%% (cdr tree) test result))
	  (if (funcall test tree)
	      (funcall result tree)
	      tree))))

(defmacro tree-leaves (tree test result)
  `(tree-leaves%%
    ,tree
    (lambda (x)
      (declare (ignore x))
      ,test)
    (lambda (x)
      (declare (ignore x))
      ,result)))

(defmacro pandoriclet (letargs &rest body)
  (let ((letargs (cons '(this)
		       (let-binding-transform letargs))))
    `(let ,letargs
       (setq this ,@(last body))
       ,@(butlast body)
       (dlambda
	(:pandoric-get (sym)
		       ,(pandoriclet-get letargs))
	(:pandoric-set (sym val)
		       ,(pandoriclet-set letargs))
	(t (&rest args)
	   (apply this args))))))

(defun pandoriclet-get (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1)) ,(car a1)) letargs)
     (t (error
	 "Unknown pandoric-get sym ~a~%" sym))))

(defun pandoriclet-set (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1)) (setq ,(car a1) val)) letargs)
     (t (error
	 "Unknown pandoric-set sym ~a~%" sym))))

(declaim (inline get-pandoric))

(defun get-pandoric (box sym)
  (funcall box :pandoric-get sym))

(defsetf get-pandoric (box sym) (val)
  `(progn
     (funcall ,box :pandoric-set ,sym ,val)
     ,val))

(defmacro! with-pandoric (syms o!box &rest body)
  `(symbol-macrolet
       (,@(mapcar #`(,a1 (get-pandoric ,g!box ',a1))
		  syms))
     ,@body))

(defun pandoric-hotpatch (box new)
  (with-pandoric (this) box
    (setq this new)))

(defmacro pandoric-recode (vars box new)
  `(with-pandoric (this ,@vars) ,box
     (setq this ,new)))

;;; plambda
(defmacro plambda (largs pargs &rest body)
  (let ((pargs (mapcar #'list pargs)))
    `(let (this self)
       (setq this (lambda ,largs ,@body)
	     self (dlambda
		   (:pandoric-get (sym)
				  ,(pandoriclet-get pargs))
		   (:pandoric-set (sym val)
				  ,(pandoriclet-set pargs))
		   (t (&rest args)
		      (apply this args)))))))

(defvar pandoric-eval-tunnel)

(defmacro pandoric-eval (vars expr)
  `(let ((pandoric-eval-tunnel
	  (plambda () ,vars t)))
     (eval `(with-pandoric
		,',vars pandoric-eval-tunnel
		,,expr))))


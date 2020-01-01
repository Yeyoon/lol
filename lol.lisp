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


(in-package #:lol)

#||
for defun defmacro/g!
useage:
(defmacro/g! nif (expr pos zero neg)
  `(let ((,g!result ,expr))
     (cond ((plusp ,g!result) ,pos)
           ((zerop ,g!result) ,zero)
            (t ,neg))))
||#


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


(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
	 (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(loop :for g :in (list ,@gs)
		    :for o :in (list ,@os)
		    :collect (append (list g) (list o)))
	  ,,@body))))


;; #` read macro
(defun |#`-reader| (stream sub-char numarg)
  (declare (ignore sub-char))
  (unless numarg (setf numarg 1))
  `(lambda ,(loop :for i :from 1 :to numarg :collect
		  (symb 'a i))
     ,(funcall (get-macro-character #\`) stream nil)))

(set-dispatch-macro-character #\# #\` #'|#`-reader|)


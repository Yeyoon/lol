#||
This is tried to parser the sdp
from the binary stram
||#

;; 1. alpha-char-p

(defun pos-digit-p (ch)
  (and (char/= ch #\0)
       (digit-char-p ch)))

;; digit-char-p

(defun alpha-numeric-p (ch)
  (or (alpha-char-p ch)
      (digit-char-p ch)))


(defun read-integer (stream)
  (let ((chars) (ch (read-char stream)))
    (unless (pos-digit-p ch)
      (unread-char ch stream)
      (return-from read-integer))

    (push ch chars)
    (do ((ch (read-char stream) (read-char stream)))
	((null ch) chars)
      (unless (digit-char-p ch)
	(unread-char ch stream)
	(return chars))
      (push ch chars))

    (format t "chars is : ~a~%" chars)
    (parse-integer
     (coerce (nreverse chars)
	     'string))))


(defun decimal-uchar (chars)
  (cond
    ((= 1 (length chars)) (digit-char-p (char chars 0)))
    ((= 2 (length chars)) (and (pos-digit-p (char chars 0))
			       (digit-char-p (char chars 1))))
    ((= 3 (length chars)) (or
			   (and (char= #\1 (char chars 0))
				(digit-char-p (char chars 1))
				(digit-char-p (char chars 2)))
			   (and (char= #\2 (char chars 0))
				(member (char chars 1) '(#\0 #\1 #\2 #\3 #\4))
				(digit-char-p (char chars 2)))
			   (and (char= #\2 (char chars 0))
				(char= #\5 (char chars 1))
				(member (char chars 2) '(#\0 #\1 #\2 #\3 #\4 #\5)))))
    (t (format t "~a is not decimal-uchar ~%" chars))))




(defun read-byte-string (stream)
  (let (chars)
    (do ((ch (read-char stream) (read-char stream)))
	((null ch) chars)
      (cond
	((null-char-p ch) (coerce (nreverse chars) 'string))
	((char= ch #\ret
       
	 

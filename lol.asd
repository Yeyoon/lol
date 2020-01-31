
;;;; lol.asd

(asdf:defsystem #:lol
  :description "This is the lib for let over lambda useful function/macros implemented in my understanding"
  :author "zhenghe <zhenghe@cisco.com>"
  :license  "public domain"
  :version "0.0.1"
  :serial t
  :depends-on ("cl-ppcre")
  :components ((:file "package")
	       (:file "utils")
	       (:file "defmacrog_util")
	       (:file "defmacrog")
	       (:file "plambda")
               (:file "lol")
	       (:file "ch8")))

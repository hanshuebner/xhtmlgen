(in-package :cl-user)

(defpackage :xhtml-generator
  (:use :common-lisp)
  (:export #:html
	   #:html-stream
           #:with-xhtml
	   #:*html-sink*))


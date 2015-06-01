(in-package :cl-user)

(asdf:defsystem :xhtmlgen
  :description "XHTML generation library"
  :author "Franz Inc"
  :license "LLGPL"
  :depends-on (:cxml)
  :serial t
  :components ((:file "package")
	       (:file "xhtmlgen")))

(asdf:defsystem :xhtmlgen-test
  :depends-on (:xhtmlgen :rt)
  :components ((:file "test")))


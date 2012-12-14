;; xhtmlgen.lisp
;; This version by david@lichteblau.com for headcraft (http://headcraft.de/)
;;
;; Derived from htmlgen.cl:
;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA 
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the AllegroServe
;; prequel found in license-allegroserve.txt.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License is in the file 
;; license-lgpl.txt that was distributed with this file.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;; Suite 330, Boston, MA  02111-1307  USA

(in-package :xhtml-generator)

(defvar *html-sink*)

;; html generation

(defstruct (html-process (:type list)
                         (:constructor make-html-process (key macro special)))
  key		; keyword naming this tag
  macro  	; the macro to define this
  special       ; if true then call this to process the keyword and return
                ; the macroexpansion
  )


(defparameter *html-process-table* 
    (make-hash-table :test #'equal) ; #'eq is accurate but want to avoid rehashes
  )

(defmacro html (&rest forms &environment env)
  ;; just emit html to the current stream
  (let ((body (gensym)))
    `(labels ((,body ()
                ,(process-html-forms forms env)))
       (if (boundp '*html-sink*)
           (,body)
           (let ((*html-sink* (cxml:make-character-stream-sink *standard-output* :canonical nil)))
             (,body)
             (sax:end-document *html-sink*))))))

(defmacro html-stream (stream &rest forms &environment env)
  `(let ((*html-sink* (cxml:make-character-stream-sink ,stream :canonical nil)))
     ,(process-html-forms forms env)
     (sax:end-document *html-sink*)))

(defmacro with-xhtml ((&optional stream &key (indentation 3 indentation-given)) &body body)
  (declare (ignore indentation))
  (when indentation-given
    (warn "WITH-XHTML: indentation argument is deprecated. It will be ignored"))
  `(let ((*html-sink* (cxml:make-character-stream-sink ,stream :canonical nil)))
     (sax:start-document *html-sink*)
     (sax:start-dtd *html-sink*
                    "html"
                    "-//W3C//DTD XHTML 1.0 Transitional//EN"
                    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd")
     (sax:end-dtd *html-sink*)
     (multiple-value-prog1
         (html
          ,@body)
      (sax:end-document *html-sink*))))

(defun get-process (form)
  (let ((ent (gethash form *html-process-table*)))
    (unless ent
      (error "unknown html keyword ~s" form))
    ent))

(defun process-html-forms (forms env)
  (let (res)
    (flet ((do-ent (ent args argsp body)
             ;; ent is an html-process object associated with the 
             ;;	    html tag we're processing
             ;; args is the list of values after the tag in the form
             ;;     ((:tag &rest args) ....)
             ;; argsp is true if this isn't a singleton tag  (i.e. it has
             ;;     a body) .. (:tag ...) or ((:tag ...) ...)
             ;; body is the body if any of the form
             ;; 
	     (let ((special (html-process-special ent)))
	       (push (if special
                         (funcall special ent args argsp body)
                         `(,(html-process-macro ent)
                           ,args
                           ,(process-html-forms body env)))
                     res))))
      (do* ((xforms forms (cdr xforms))
	    (form (car xforms) (car xforms)))
	  ((null xforms))

	(setq form (macroexpand form env))
	
	(if (atom form)
            (typecase form
              (keyword (do-ent (get-process form) nil nil nil)) 
              (string (push `(sax:characters *html-sink* ,form) res))
              (t (push form res)))
            (let ((first (car form)))
              (cond
                ((keywordp first)
                  ;; (:xxx . body) form
                  (do-ent (get-process (car form)) nil t (cdr form)))
                 ((and (consp first) (keywordp (car first)))
                   ;; ((:xxx args ) . body)
                   (do-ent (get-process (caar form)) (cdr first) t (cdr form)))
                (t
                  (push form res)))))))
    `(progn ,@(nreverse res))))

(defun html-body-key-form (string-code args body &key self-close-if-empty-p)
  (unless (evenp (length args))
    (error "attribute list ~S isn't even" args))
  `(let ((.tagname. ,string-code))
     (sax:start-element *html-sink* nil nil .tagname.
                        (list
                         ,@(loop
                               for (name value) on args by #'cddr
                               collect
                                 `(sax:make-attribute
                                   :qname ,(etypecase name
					; fixme: all attribute names converted to lower case, this won't work
					; all the time.
                                             (symbol (string-downcase (symbol-name name)))
                                             (string name))
                                   :value (format nil "~A" ,value)
                                   :specified-p t))))
     ,@(unless self-close-if-empty-p
        `((cxml::maybe-close-tag *html-sink*)))
     ,@body
     (sax:end-element *html-sink* nil nil .tagname.)))

(defun emit-without-quoting (str)
  ;; das ist fuer WPDISPLAY
  (cxml::maybe-close-tag *html-sink*)
  (cxml::sink-write-rod str *html-sink*))

(defun princ-http (val)
  #+(or)
  (warn "use of deprecated :PRINC (use :PRINC-SAFE instead?)")
  (emit-without-quoting (princ-to-string val)))

(defun prin1-http (val)
  #+(or)
  (warn "use of deprecated :PRIN1 (use :PRIN1-SAFE instead?)")
  (emit-without-quoting (prin1-to-string val)))

(defun princ-safe-http (val)
  (sax:characters *html-sink* (princ-to-string val)))

(defun prin1-safe-http (val)
  (sax:characters *html-sink* (prin1-to-string val)))


;; --  defining how html tags are handled. --
;;
;; most tags are handled in a standard way and the def-std-html
;; macro is used to define such tags
;;
;; Some tags need special treatment and def-special-html defines
;; how these are handled.  The tags requiring special treatment
;; are the pseudo tags we added to control operations
;; in the html generator.
;; 
;;
;; tags can be found in three ways:
;;  :br	    		- singleton, no attributes, no body
;;  (:b "foo")          - no attributes but with a body
;;  ((:a href="foo") "balh")  - attributes and body
;;
  
(defmacro def-special-html (kwd fcn)
  ;; kwd - the tag we're defining behavior for.
  ;; fcn - function to compute the macroexpansion of a use of this
  ;;       tag. args to fcn are: 
  ;;		ent - html-process object holding info on this tag
  ;;		args - list of attribute-values following tag
  ;;		argsp - true if there is a body in this use of the tag
  ;;		body - list of body forms.
  `(setf (gethash ,kwd *html-process-table*) 
     (make-html-process ,kwd nil ,fcn)))

(def-special-html :newline 
    #'(lambda (ent args argsp body)
	(declare (ignore ent args argsp))
	(when body
          (error "can't have a body with :newline -- body is ~s" body))
	(emit-without-quoting (string #\newline))))
			       
(def-special-html :princ 
    #'(lambda (ent args argsp body)
	(declare (ignore ent args argsp))
	`(progn ,@(mapcar #'(lambda (bod)
			      `(princ-http ,bod))
			  body))))

(def-special-html :princ-safe 
    #'(lambda (ent args argsp body)
	(declare (ignore ent args argsp))
	`(progn ,@(mapcar #'(lambda (bod)
			      `(princ-safe-http ,bod))
			  body))))

(def-special-html :prin1 
    #'(lambda (ent args argsp body)
	(declare (ignore ent args argsp))
	`(progn ,@(mapcar #'(lambda (bod)
			      `(prin1-http ,bod))
			  body))))

(def-special-html :prin1-safe 
    #'(lambda (ent args argsp body)
	(declare (ignore ent args argsp))
	`(progn ,@(mapcar #'(lambda (bod)
			      `(prin1-safe-http ,bod))
			  body))))

(def-special-html :format
    ;; example:
    ;; (html (:p (:format "hi ~A!" "paul")))
    #'(lambda (ent args argsp body)
	(declare (ignore ent args argsp))
	`(princ-http (format nil ,@body))))

(def-special-html :format-safe
    #'(lambda (ent args argsp body)
	(declare (ignore ent args argsp))
	`(princ-safe-http (format nil ,@body))))

(def-special-html :comment
  #'(lambda (ent args argsp body)
      (declare (ignore ent args argsp body))
      `(warn ":COMMENT in html macro not supported yet")))

(defmacro def-std-html (kwd &key (self-close-if-empty-p t))
  (let ((mac-name (intern (format nil "~a-~a" :with-html kwd)))
	(string-code (string-downcase (string kwd))))
    `(progn (setf (gethash ,kwd *html-process-table*)
	      (make-html-process ,kwd ',mac-name nil))
	    (defmacro ,mac-name (args &rest body)
	      (html-body-key-form ,string-code args body :self-close-if-empty-p ,self-close-if-empty-p)))))

(def-std-html :a)
(def-std-html :abbr)
(def-std-html :acronym)
(def-std-html :address)
(def-std-html :applet)
(def-std-html :area)

(def-std-html :b)
(def-std-html :base)
(def-std-html :basefont)
(def-std-html :bdo)
(def-std-html :bgsound)
(def-std-html :big)
(def-std-html :blink)
(def-std-html :blockquote)
(def-std-html :body)
(def-std-html :br)
(def-std-html :button)

(def-std-html :caption)
(def-std-html :center)
(def-std-html :cite)
(def-std-html :code)
(def-std-html :col)
(def-std-html :colgroup)

(def-std-html :dd)
(def-std-html :del)
(def-std-html :dfn)
(def-std-html :dir)
(def-std-html :div)
(def-std-html :dl)
(def-std-html :dt)

(def-std-html :em)
(def-std-html :embed)

(def-std-html :fieldset)
(def-std-html :font)
(def-std-html :form)
(def-std-html :frame)
(def-std-html :frameset)

(def-std-html :h1)
(def-std-html :h2)
(def-std-html :h3)
(def-std-html :h4)
(def-std-html :h5)
(def-std-html :h6)
(def-std-html :head)
(def-std-html :hr)
(def-std-html :html)

(def-std-html :i)
(def-std-html :iframe :self-close-if-empty-p nil)
(def-std-html :ilayer)
(def-std-html :img)
(def-std-html :input)
(def-std-html :ins)
(def-std-html :isindex)

(def-std-html :kbd)
(def-std-html :keygen)

(def-std-html :label)
(def-std-html :layer)
(def-std-html :legend)
(def-std-html :li)
(def-std-html :link)
(def-std-html :listing)

(def-std-html :map)
(def-std-html :marquee)
(def-std-html :menu)
(def-std-html :meta)
(def-std-html :multicol)

(def-std-html :nobr)
(def-std-html :noembed)
(def-std-html :noframes)
(def-std-html :noscript)

(def-std-html :object)
(def-std-html :ol)
(def-std-html :optgroup)
(def-std-html :option)

(def-std-html :p)
(def-std-html :param)
(def-std-html :plaintext)
(def-std-html :pre)

(def-std-html :q)

(def-std-html :s)
(def-std-html :samp)
(def-std-html :script :self-close-if-empty-p nil)
(def-std-html :select)
(def-std-html :server)
(def-std-html :small)
(def-std-html :spacer)
(def-std-html :span)
(def-std-html :strike)
(def-std-html :strong)
(def-std-html :style)  
(def-std-html :sub)
(def-std-html :sup)

(def-std-html :table)
(def-std-html :tbody)
(def-std-html :td)
(def-std-html :textarea :self-close-if-empty-p nil)
(def-std-html :tfoot)
(def-std-html :th)
(def-std-html :thead)
(def-std-html :title)
(def-std-html :tr)
(def-std-html :tt)

(def-std-html :u)
(def-std-html :ul)

(def-std-html :var)

(def-std-html :wbr)

(def-std-html :xmp)

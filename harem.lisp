;; (C) 2013 IBM Corporation
;;  Author: Alexandre Rademaker
;;
;; Referencias:
;; - http://code.google.com/p/cl-en/source/browse/trunk/basics.lisp#148
;; - http://www.ibm.com/developerworks/xml/tutorials/x-usax/
;; - http://common-lisp.net/project/cxml/
;; - http://common-lisp.net/project/cxml/saxoverview/index.html

(in-package :harem)

(defvar *state* nil "identify current state")

(defun state-p (id)
  (member id *state*))

(defun state-on (&rest ids)
  (setf *state* (union ids *state*)))

(defun state-off (&rest ids)
  (setf *state* (if (null ids) nil
		    (set-difference *state* ids))))


(defclass document ()
  ((id       :initform nil :initarg :id :accessor doc-id)
   (stack    :initform nil :accessor doc-stack)))

(defclass alternative ()
  ((current  :initform nil :accessor alternative-current)
   (stacks   :initform nil :accessor alternative-stacks)))

(defclass mention ()
  ((id      :initform nil)
   (stack   :initform nil :accessor mention-stack)
   (categ   :initform nil)
   (tipo    :initform nil)
   (subtipo :initform nil)
   (comment :initform nil)))

(defclass harem-handler (sax:default-handler)
  ((offset   :initform nil :accessor hh-offset)
   (docs     :initform nil :accessor hh-docs) 
   (stack    :initform nil :accessor hh-stack)))


(defmethod sax:start-element ((h harem-handler) (namespace t) (local-name t) (qname t) 
			      (attributes t))
  (with-slots (docs stack) h
    (cond 
      ((equal local-name "ALT")
       (state-on :reading-alt)
       (push (make-instance 'alternative) stack))
      ((equal local-name "EM")
       (state-on :reading-em)
       (let ((em (make-instance 'mention)))
	 (dolist (attr '(("ID" id) ("CATEG" categ) ("TIPO" tipo) 
			 ("COMENT" comment) ("SUBTIPO" subtipo)))
	   (let ((at (sax:find-attribute (car attr) attributes)))
	     (if at 
		 (setf (slot-value em (cadr attr)) (sax:attribute-value at)))))
	 (if (state-p :reading-alt) 
	     (with-slots (current) (car stack) 
	       (push em current))
	     (push em stack))))
      ((equal local-name "OMITIDO")
       (state-on :reading-omitted))
      ((equal local-name "DOC") 
       (let ((id (sax:attribute-value (sax:find-attribute "DOCID" attributes))))
	 (push (make-instance 'document :id id) docs))))))


(defmethod sax:end-element ((h harem-handler) (namespace t) (local-name t) (qname t))
  (with-slots (docs stack) h
    (cond 
      ((equal local-name "DOC") 
       (setf (slot-value (car docs) 'stack) (reverse stack))
       (setf stack nil))
      ((equal local-name "p")
       (push (format nil "~%~%") stack))
      ((equal local-name "bar")
       (with-slots (current stacks) (car stack)
	 (push (reverse current) stacks)
	 (setf current nil)))
      ((equal local-name "OMITIDO")
       (state-off :reading-omitted))
      ((equal local-name "EM")
       (state-off :reading-em))
      ((equal local-name "ALT")
       (with-slots (current stacks) (car stack)
	 (push (reverse current) stacks)
	 (setf current nil
	       stacks (reverse stacks)))
       (state-off :reading-alt)))))


(defun normalize-string (data)
  (labels ((norm (re data)
	     (cl-ppcre:regex-replace-all re data " ")))
    (norm "  +" (norm "\\n+" data))))


(defmethod sax:characters ((h harem-handler) (data t))
  (with-slots (stack) h
    (let ((chunk (string-trim '(#\Newline #\Space #\Tab) data))) 
      (unless (equal chunk "")
	(let ((blk (normalize-string data)))
	  (cond 
	    ((state-p :reading-em)
	     (let ((top (car stack))) 
	       (if (state-p :reading-alt)
		   (push blk (mention-stack (car (alternative-current top))))
		   (push blk (mention-stack top)))))
	    ((state-p :reading-alt)
	     (with-slots (current) (car stack)
	       (push blk current)))
	    (t
	     ;; (if (and (stringp (car stack))
	     ;; 	      (not (cl-ppcre:scan " $" (car stack)))
	     ;; 	      (not (cl-ppcre:scan "^ " blk))) 
	     ;; 	 (push " " stack))
	     (push blk stack))))))))


(defun save-doc (doc &key (directory #P"corpus/"))
  (let* ((filename (pathname (format nil "~a.txt" (doc-id doc))))
	 (fullpath (cl-fad:merge-pathnames-as-file directory filename)))
    (with-open-file (out fullpath :direction :output :if-exists :supersede)
      (dolist (data (doc-stack doc)) 
	(cond 
	  ((stringp data)
	   (write-string data out))
	  ((equal 'mention (type-of data))
	   (mapcar #'(lambda (str) (write-string str out)) 
		   (reverse (slot-value data 'stack)))))))))


(defun load-harem (filename) 
  (let ((hh (make-instance 'harem-handler))
	(*state* nil))
    (cxml:parse filename hh)
    (reverse (slot-value hh 'docs))))

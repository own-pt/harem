;; (C) 2013 IBM Corporation
;;  Author: Alexandre Rademaker
;;                          _
;;  _._ _..._ .-',     _.._(`))
;; '-. `     '  /-._.-'    ',/
;;    )         \            '.
;;   / _    _    |             \
;;  |  a    a    /              |
;;  \   .-.                     ;  
;;   '-('' ).-'       ,'       ;
;;      '-;           |      .'
;;         \           \    /
;;         | 7  .__  _.-\   \
;;         | |  |  ``/  /`  /
;;        /,_|  |   /,_/   /
;;           /,_/      '`-'
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
    (norm "  +" (norm "(\\n|\\t)+" data))))


(defmethod sax:characters ((h harem-handler) (data t))
  (with-slots (stack) h
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
	 (push blk stack))))))


;; this is the function that actually save the text and the mentions
;; in two separated files. Mentions must be associated with character
;; offsets (begin, end) where they occur in the text. alternative
;; blocks need a special threatment, only one possiblility should be
;; save in the text but all mentions from all possibilities but be
;; saved in the mentions file.

(defun save-alternative (obj data-stream meta-stream &key (start 0))
  (let ((saved (+ start (file-position data-stream))))
    (loop for stack in (alternative-stacks obj)
	  for dummy = nil then (make-string-output-stream)
	  for first? = t then nil 
	  do 
	  (if first? 
	      (save-stack stack data-stream meta-stream :start start)
	      (save-stack stack dummy meta-stream :start saved)))))


(defun save-mention-data (stream start end label id comment categ tipo subtipo)
  (if (listp tipo)
      (assert (= (length categ) (length tipo))))
  (dotimes (n (length categ))
    (let ((cat (or (nth n categ) nil))
	  (tip (or (nth n tipo) nil))
	  (sub (or (nth n subtipo) nil)))
      (fare-csv:write-csv-line (list start end id label cat tip sub comment) stream))))


(defun save-mention (obj data-stream meta-stream &key (start 0))
  (let* ((start (+ 1 start (file-position data-stream)))
	 (data-stream-label (make-string-output-stream))
	 (data-broadcast (make-broadcast-stream data-stream data-stream-label)))
    (save-stack (reverse (mention-stack obj)) data-broadcast meta-stream :start start)
    (save-mention-data meta-stream
		       start (+ start (file-position data-stream))
		       (get-output-stream-string data-stream-label)
		       (slot-value obj 'id)
		       (slot-value obj 'comment)
		       (cl-ppcre:split "\\|" (slot-value obj 'categ))
		       (cl-ppcre:split "\\|" (slot-value obj 'tipo))
		       (cl-ppcre:split "\\|" (slot-value obj 'subtipo)))))


(defun save-stack (stack data-stream meta-stream &key (start 0))
  (if (null stack)
      (file-position data-stream)
      (let ((data (car stack))) 
	(cond 
	  ((stringp data)
	   (write-string data data-stream))
	  ((equal 'alternative (type-of data))
	   (save-alternative data data-stream meta-stream :start start))
	  ((equal 'mention (type-of data))
	   (save-mention data data-stream meta-stream :start start)))
	(save-stack (cdr stack) data-stream meta-stream :start start))))


(defun save-doc (doc &key (directory #P"corpus/"))
  (flet ((fp (ext)
	   (let ((filename (pathname (format nil "~a.~a" (doc-id doc) ext))))
	     (cl-fad:merge-pathnames-as-file directory filename))))
    (let ((out (make-string-output-stream)))
      (with-open-file (outf (fp "txt") :direction :output :if-exists :supersede)
	(with-open-file (meta (fp "dat") :direction :output :if-exists :supersede)
	  (save-stack (doc-stack doc) out meta :start 0)
	  (format outf (get-output-stream-string out)))))))


(defun load-harem (filename) 
  (let ((hh (make-instance 'harem-handler))
	(*state* nil))
    (cxml:parse filename hh)
    (reverse (slot-value hh 'docs))))

;; (C) 2013 IBM Corporation
;;
;;  Author: Alexandre Rademaker
;; Project: Watson Multi Language
;;
;; Referencias:
;; - http://code.google.com/p/cl-en/source/browse/trunk/basics.lisp#148
;; - http://www.ibm.com/developerworks/xml/tutorials/x-usax/
;; - http://common-lisp.net/project/cxml/
;; - http://common-lisp.net/project/cxml/saxoverview/index.html

(in-package :wml)


(defclass document ()
  ((id :initform nil)
   (text :initform nil)
   (mentions :initform nil)))


(defun synset-br2en (ss-br)
  (let* ((id (slot-value ss-br 'id))
	 (id-pos (subseq id 0 1))
	 (id-offset (format nil "~8,'0d" (parse-integer (subseq id 1))))
	 (words nil))    
    (dolist (w (cl-ppcre:split "\\s*(,|;)\\s*" 
			       (or (slot-value ss-br 'words-man)
				   (slot-value ss-br 'words-sug))))
      (pushnew (list (string-trim '(#\Space #\Tab) w) nil nil) words :key 'car :test 'equal))
    (make-instance 'synset 
		   :id id-offset
		   :ss-type id-pos
		   :base (not (string= (slot-value ss-br 'bc) "99999999"))
		   :words words
		   :gloss (or (slot-value ss-br 'gloss-man) 
			      (slot-value ss-br 'gloss-sug))
		   :notes (slot-value ss-br 'comments))))


(defclass sax-handler (sax:default-handler)
  ((cdoc     :initform nil :reader cdoc)
   (cmention :initform nil :reader cmention)
   (tmention :initform nil :reader tmention)
   (offset   :initform nil :reader offset)
   (docs     :initform nil :reader docs) 
   (stack    :initform nil :reader collected-text)))


(defmethod sax:start-element ((h sax-handler) (namespace t) (local-name t) (qname t) (attributes t))
  (with-slots (curdoc curmention stack offset) h
    (cond 
      ((equal local-name "doc") 
       (setf curdoc (make-instance 'document)))
      ((equal local-name "alt") 
       (setf curmention )
       (setf stack nil))
      ((equal local-name "em") 
       (setf cmention (list offset attributes))
       (setf tmention nil)))))


(defmethod sax:end-element ((h sax-handler) (namespace t) (local-name t) (qname t))
  (with-slots (current-ss current-field stack synsets) h
    (cond 
      ((equal local-name "row") 
       (push current-ss synsets))
      ((assoc local-name *xml-fields* :test 'equal) 
       (if (> (length stack) 0)
	   (setf (slot-value current-ss current-field) (format nil "~{~A~}" (reverse stack))))
       (setf stack nil)))))


(defmethod sax:characters ((h sax-handler) (data t))
  (with-slots (stack) h
    (push data stack)))


(defun load-harem (filename) 
  (let ((my (make-instance 'sax-handler)))
    (cxml:parse filename my)
    (mapcar #'save-doc (slot-value my 'docs))))

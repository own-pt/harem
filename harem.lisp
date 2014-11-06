;; (C) 2013 IBM Corporation
;;  Author: Alexandre Rademaker
;;
;; Referencias:
;; - http://code.google.com/p/cl-en/source/browse/trunk/basics.lisp#148
;; - http://www.ibm.com/developerworks/xml/tutorials/x-usax/
;; - http://common-lisp.net/project/cxml/
;; - http://common-lisp.net/project/cxml/saxoverview/index.html

(in-package :harem)


(defclass document ()
  ((id       :initform nil :initarg :id :accessor id)
   (text     :initform nil :accessor text)
   (mentions :initform nil :accessor mentions)))

(defclass harem-handler (sax:default-handler)
  ((curdoc   :initform nil :accessor curdoc)
   (cmention :initform nil :reader cmention)
   (tmention :initform nil :reader tmention)
   (offset   :initform nil :reader offset)
   (docs     :initform nil :accessor docs) 
   (stack    :initform nil :reader collected-text)))


(defmethod sax:start-element ((h harem-handler) (namespace t) (local-name t) (qname t) (attributes t))
  (with-slots (curdoc docs) h
    (cond 
      ((equal local-name "DOC") 
       (let ((id (sax:attribute-value (sax:find-attribute "DOCID" attributes))))
	 (setf curdoc (make-instance 'document :id id)))))))


(defmethod sax:end-element ((h harem-handler) (namespace t) (local-name t) (qname t))
  (with-slots (curdoc docs) h
    (cond 
      ((equal local-name "DOC") 
       (push curdoc docs)
       (setf curdoc nil)))))


;; (defmethod sax:characters ((h harem-handler) (data t))
;;   ; escrever o data dentro do stream do doc corrent (se nao estou
;;   ; dentro de um alt depois do primeiro |)
;;   (with-slots (cdoc) h
;;     (push data stack)))

;; (defun save-doc (doc)
;;   ...)

(defun load-harem (filename) 
  (let ((my (make-instance 'harem-handler)))
    (cxml:parse filename my)
    (slot-value my 'docs)))


(in-package :harem)

;; I could have used the cxml:broadcast-handler class too. Actually,
;; sax-proxy is a subclass of broadcast-handler.

(defclass preharem (cxml:sax-proxy) 
  ((state :initform nil :accessor preproc-state)))


(defmethod sax:start-element ((h preharem) (namespace t) (local-name t) 
			      (qname t) (attributes t))
  (if (equal "ALT" local-name)
      (setf (slot-value h 'state) 'reading-alt))
  (call-next-method))


(defmethod sax:end-element ((h preharem) (namespace t) (local-name t) (qname t))
  (if (equal local-name "ALT")
      (setf (slot-value h 'state) nil))
  (call-next-method))


(defmethod sax:characters ((h preharem) data)
  (if (equal (slot-value h 'state) 'reading-alt)
      (let ((chunks (cl-ppcre:split "\\|" data)))
	(if (= 1 (length chunks))
	    (call-next-method)
	    (loop for c in chunks
		  for first? = t then nil
		  do (unless first?
		       (sax:start-element h nil nil "bar" nil)
		       (sax:end-element h nil nil "bar"))
		  (sax:characters h c))))
      (call-next-method)))


(defun preproc-harem (input output) 
  (with-open-file (out output :if-exists :supersede :direction :output)
    (flet ((resolver (pubid sysid)
	     (declare (ignore pubid sysid))
	     (flexi-streams:make-in-memory-input-stream nil)))
      (let ((h (make-instance 'preharem 
			      :chained-handler (cxml:make-character-stream-sink out))))
	(cxml:parse input h :validate nil :entity-resolver #'resolver)))))



(in-package :harem)

;; I could have used the cxml:broadcast-handler class too. Actually,
;; sax-proxy is a subclass of broadcast-handler.

(defclass preproc (cxml:sax-proxy) ())

(defmethod sax:characters ((handler preproc) data)
  (let ((chunks (cl-ppcre:split "\\|" data)))
    (if (= 1 (length chunks))
	(call-next-method)
	(loop for c in chunks
	      for first? = t then nil
	      do (unless first?
		   (sax:start-element handler nil nil "bar" nil)
		   (sax:end-element handler nil nil "bar"))
	      (sax:characters handler c)))))


(defun preproc-harem (input output) 
  (with-open-file (out output :if-exists :supersede :direction :output)
    (flet ((resolver (pubid sysid)
	     (declare (ignore pubid sysid))
	     (flexi-streams:make-in-memory-input-stream nil)))
      (let ((h (make-instance 'preproc 
			      :chained-handler (cxml:make-character-stream-sink out))))
	(cxml:parse input h :validate nil :entity-resolver #'resolver)))))


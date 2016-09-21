
(in-package :harem)

(defmacro with-open-files (args &body body)
  (case (length args)
    ((0)
     `(progn ,@body))
    ((1)
     `(with-open-file ,(first args) ,@body))
    (t `(with-open-file ,(first args)
	  (with-open-files
	      ,(rest args) ,@body)))))


;; code used during tests:
;;
;; (with-open-file (out #P"teste.xml" :if-exists :supersede :direction :output)
;;   (let ((h (make-instance 'preproc
;; 			  :chained-handler (cxml:make-character-stream-sink out))))
;;     (cxml:parse #P"CDSegundoHAREMclassico.xml" h :validate t)))
;;
;; (with-open-file (out #P"teste.xml" :if-exists :supersede :direction :output)
;;   (let* ((h (make-instance 'preproc :chained-handler (cxml-dom:make-dom-builder)))
;; 	 (dom (cxml:parse #P"CDSegundoHAREMclassico.xml" h :validate t)))
;;     (dom:map-document out dom :include-doctype nil)))
;;
;; (with-open-file (out #P"teste.xml" :if-exists :supersede :direction :output)
;;   (let ((h (make-instance 'preproc
;; 			  :handlers (list (cxml:make-character-stream-sink out)))))
;;     (cxml:parse #P"CDSegundoHAREMclassico.xml" h :validate t)))

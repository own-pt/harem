
(in-package :harem)

(defun compose (f g)
  #'(lambda (x) (funcall f (funcall g x))))


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

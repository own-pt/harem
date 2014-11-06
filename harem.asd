
(asdf:defsystem #:harem
  :serial t
  :depends-on (#:cxml :cl-ppcre)
  :components ((:file "package")
	       (:file "pre-harem" :depends-on ("package"))
	       (:file "harem" :depends-on ("package"))))


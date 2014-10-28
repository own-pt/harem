
(asdf:defsystem #:wml
  :serial t
  :depends-on (#:agclient #:cxml)
  :components ((:file "package")
	       (:file "harem" :depends-on ("package"))))



(asdf:defsystem #:harem
  :serial t
  :depends-on (#:cxml #:cl-ppcre #:flexi-streams #:cl-fad)
  :components ((:file "package")
	       (:file "pre-harem" :depends-on ("package"))
	       (:file "harem" :depends-on ("package"))))


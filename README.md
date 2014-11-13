
# Common Lisp parser for HAREM Collection

The HAREM collection is available at
http://www.linguateca.pt/harem/. This package is a CL parser for HAREM
CDSegundoHAREMclassico.xml file.

More info soon.

The DTD can be used to validate the XML with:

    xmllint --noout --debug --dtdvalid harem.dtd CDSegundoHAREMclassico.xml

There are few mistakes in the CDSegundoHAREMclassico.xml that I fixed
manually before validation. The package implements two SAX
parsers. The first one is responsable for transform all occurrence of
"|" in text blocks inside <alt> tag to <bar/> tag. The second parser
generate two text files for each document in the
CDSegundoHAREMclassico.xml: one file with the raw text and other with
the mentions and its character offsets.


   

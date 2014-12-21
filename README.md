
# Common Lisp parser for HAREM Collection

The HAREM collection is available at
http://www.linguateca.pt/harem/.

This library is a CL parsers for HAREM CDSegundoHAREMclassico.xml
file. I have also developed an DTD file that can be used to validate
the XML with:

    xmllint --noout --debug --dtdvalid harem.dtd CDSegundoHAREMclassico.xml

There are few mistakes in CDSegundoHAREMclassico.xml that I fixed
manually with the help of the output of the command above.

This library implements two SAX parsers. The first one is responsable
for transform all occurrence of "|" in text blocks inside ```<alt>```
tag to ```<bar/>``` tag. See function ```preproc-harem```. This
pre-processing turns easier the transformation of the document.

The second part aims to generate two text files for each document in
the XML file produced with ```preproc-harem```. One file with the raw
text and other with the mentions and its character offsets, see
functions ```load-harem``` and ```save-doc```.  The first function
returns a list of documents retrieved from the XML file. The second
function save each document into the two text files.

## How to use it?

    cl-user> (ql:quickload :harem)
    cl-user> (in-package :harem)


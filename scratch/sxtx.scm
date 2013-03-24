(use ssax sxpath txpath)

(define nsmap1 '((#f . "http://www.w3.org/1999/xhtml")
                 (fubar . "http://fu.bar.com/ns")))

;; This document is just for reference. Reading 'xmldoc' with ssax:xml->sxml, using nsmap1,
;; should produce the following sxml tree.
(define doc1
  '(*TOP*
     (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml") (fubar "http://fu.bar.com/ns")))
     (html
       (head
         (title "My Test Document")
         (fubar:meta
           (fubar:created-date "2013-03-23")
           (fubar:license "Creative Commons")))
       (body
         (@ (class "fubar"))
         (h1 "Hi there!")
         (p
           (@ (class "boilerplate"))
           "Lorem Ipsum etc. etc.")))))

;; This document is just for reference. Reading 'xmldoc' with ssax:xml->sxml with an empty
;; namespace prefix declaration should produce the following sxml tree.
(define doc3
  '(*TOP*
     (http://www.w3.org/1999/xhtml:html
       (http://www.w3.org/1999/xhtml:head
         (http://www.w3.org/1999/xhtml:title "My Test Document")
         (http://fu.bar.com/ns:meta
           (http://fu.bar.com/ns:created-date "2013-03-23")
           (http://fu.bar.com/ns:license "Creative Commons")))
       (http://www.w3.org/1999/xhtml:body
         (@ (class "fubar"))
         (http://www.w3.org/1999/xhtml:h1 "Hi there!")
         (http://www.w3.org/1999/xhtml:p
           (@ (class "boilerplate"))
           "Lorem Ipsum etc. etc.")))))

(define xmldoc #<<XMLDOC
<html xmlns="http://www.w3.org/1999/xhtml" xmlns:fubar="http://fu.bar.com/ns">
  <head>
    <title>My Test Document</title>
    <fubar:meta>
      <fubar:created-date>2013-03-23</fubar:created-date>
      <fubar:license>Creative Commons</fubar:license>
    </fubar:meta>
  </head>
  <body class="fubar">
    <h1>Hi there!</h1>
    <p class="boilerplate">Lorem Ipsum etc. etc.</p>
  </body>
</html>
XMLDOC
)

(define doc2
  (with-input-from-string xmldoc
    (lambda ()
      (ssax:xml->sxml (current-input-port) nsmap1))))

(define doc4
  (with-input-from-string xmldoc
    (lambda ()
      (ssax:xml->sxml (current-input-port) '()))))


(define nsmap2 (list (cons '*default* (cdar nsmap1)) (cadr nsmap1)))

;; Select a prefixed element.
(define sx1 (sxpath '(// fubar:license) nsmap2))

;; sx1 and tx1 should be equivalent
(define tx1 (txpath "//fubar:license" nsmap2))

;; Select an attribute of a non-prefixed element.
(define sx2 (sxpath '(// p @ class) nsmap2))

;; sx2 and tx2 should be equivalent
(define tx2 (txpath "//p/@class" nsmap2))


(if (equal? doc1 doc2)
  (print "doc1 = doc2")
  (print "doc1 != doc2"))

(if (equal? doc3 doc4)
  (print "doc3 = doc4")
  (print "doc3 != doc4"))


(print "DOC 2/TEST 1")
(print "sxpath:")
(pp (sx1 doc2))
(print "txpath:")
(pp (tx1 doc2))

(print "DOC 2/TEST 2")
(print "sxpath:")
(pp (sx2 doc2))
(print "txpath:")
(pp (tx2 doc2))

(print "DOC 4/TEST 1")
(print "sxpath:")
(pp (sx1 doc4))
(print "txpath:")
(pp (tx1 doc4))

(print "DOC 4/TEST 2")
(print "sxpath:")
(pp (sx2 doc4))
(print "txpath:")
(pp (tx2 doc4))

(use ssax sxpath txpath)

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

;; Reading the above 'xmldoc' with ssax:xml->sxml should produce the following sxml tree
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

(define nsmap1 '((#f . "http://www.w3.org/1999/xhtml")
                 (fubar . "http://fu.bar.com/ns")))

(define nsmap2 (list (cons '*default* (cdar nsmap1)) (cadr nsmap2)))

(define sx1 (sxpath '(// fubar:license) nsmap2))

(define tx1 (txpath "//fubar:license" nsmap2))

(define sx2 (sxpath '(// p @ class) nsmap2))

(define tx2 (txpath "//p/@class" nsmap2))

(define doc2
  (with-input-from-string xmldoc
    (lambda ()
      (ssax:xml->sxml (current-input-port) nsmap1))))

(if (equal? doc1 doc2)
  (print "doc1 = doc2")
  (print "doc1 != doc2"))

(print "TEST 1")
(print "sxpath:")
(pp (sx1 doc2))
(print "txpath:")
(pp (tx1 doc2))

(print "TEST 2")
(print "sxpath:")
(pp (sx2 doc2))
(print "txpath:")
(pp (tx2 doc2))

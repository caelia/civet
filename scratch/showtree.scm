(use ssax)

(define nsmap
  '((#f . "http://www.w3.org/1999/xhtml")
    (cvt . "http://xmlns.therebetygers.net/civet/0.1")))

(define (show-tree t)
  (print "[TREE]\n")
  (pp t)
  (if (or (null? t) (string? t) (symbol? t))

    (print "\n==============================================\n")

    (let ((head (car t)) (tail (cdr t)))

      (print "\n----------------------------------------------\n")
      (print "[HEAD]\n")
      (pp head)
      (print "\n----------------------------------------------\n")
      (print "[TAIL]\n")
      (pp tail)
      (print "\n==============================================\n")

      (when (list? head) (show-tree head))
      (show-tree tail))))

(define (run file)
  (let ((doc
          (with-input-from-file
            file
            (lambda ()
              (ssax:xml->sxml (current-input-port) nsmap)))))
    (show-tree doc)))

(run (cadr (argv)))


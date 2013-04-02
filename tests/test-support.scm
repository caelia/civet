(use srfi-13)

;; The first two functions are used by both alist=? and att-list=?
(define (symbol<? s1 s2)
  (and (symbol? s1)
       (symbol? s2)
       (string<? (symbol->string s1) (symbol->string s2))))

(define (head-sym<? a b)
  (and (pair? a)
       (pair? b)
       (symbol<? (car a) (car b))))

;; This function finds two alists equal if and only if they contain all
;;   the same associations in any order.
(define (alist=? al1 al2)
  (equal?
    (sort al1 head-sym<?)
    (sort al2 head-sym<?)))

;; The SXML=? function finds two SXML objects equal if they contain all
;;   the same elements, annotations and text nodes in the same order,
;;   and where all attribute lists are equal regardless of order. If
;;   IGNORE-WHITESPACE is #t, text nodes are compared with leading &
;;   trailing whitespace removed.
(define (text-node=? tn1 tn2 ignore-whitespace)
  (and (string? tn1)
       (string? tn2)
       (or (string=? tn1 tn2)
           (and ignore-whitespace
                (string=? (string-trim-both tn1) (string-trim-both tn2))))))

(define (att-list=? lst1 lst2)
  (equal? (sort lst1 head-sym<?) (sort lst2 head-sym<?)))

(define (sxml=? node1 node2 #!optional (ignore-whitespace #t))
  (cond
    ((and (string? node1) (string? node2)) (text-node=? node1 node2 ignore-whitespace))
    ((and (pair? node1) (pair? node2)
          (eqv? '@ (car node1)) (eqv? '@ (car node2)))
     (att-list=? (cdr node1) (cdr node2)))
    ((and (pair? node1) (pair? node2))
     (and (sxml=? (car node1) (car node2) ignore-whitespace)
          (sxml=? (cdr node1) (cdr node2) ignore-whitespace)))
    (else (equal? node1 node2))))

(define (block-data-alist=? a1 a2)
  (let* ((sa1 (sort a1 head-sym<?))
         (sa2 (sort a2 head-sym<?))
         (var=?
           (lambda (pair1 pair2)
             (and (eqv? (car pair1) (car pair2))
                  (string=? (string-trim-both (cdr pair1))
                            (string-trim-both (cdr pair2))))))
         (var-list=?
           (lambda (lst1* lst2*)
             (let loop ((lst1 lst1*)
                        (lst2 lst2*))
               (or (and (null? lst1) (null? lst2))
                   (and (var=? (car lst1) (car lst2))
                        (loop (cdr lst1) (cdr lst2))))))))
    (let loop ((a1* sa1) (a2* sa2))
      (cond
        ((and (null? a1*) (null? a2*)) #t)
        ((or (null? a1*) (null? a2*)) #f)
        (else
          (let* ((head1 (car a1*))
                 (head2 (car a2*))
                 (key1 (car head1))
                 (key2 (car head2))
                 (loc1 (cadr head1))
                 (loc2 (cadr head2))
                 (vars1 (caddr head1))
                 (vars2 (caddr head2))
                 (blox1 (cadddr head1))
                 (blox2 (cadddr head2)))
            (and (eqv? key1 key2)
                 (var-list=? loc1 loc2)
                 (var-list=? vars1 vars2)
                 (sxml=? blox1 blox2)
                 (loop (cdr a1*) (cdr a2*)))))))))

(define (block-data=? bd1 bd2)
  (and (sxml=? (car bd1) (car bd2))
       (block-data-alist=? (cadr bd1) (cadr bd2))))

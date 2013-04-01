(use srfi-13)

;; This function finds two alists equal if and only if they contain all
;;   the same associations in any order.
(define (alist=? al1 al2)
  (and (= (length al1) (length al2))
       (foldl
         (lambda (seed elt)
           (and seed
                (equal? (assoc (car elt) al2) elt)))
         #t
         al1)))

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

(define (symbol<? s1 s2)
  (and (symbol? s1)
       (symbol? s2)
       (string<? (symbol->string s1) (symbol->string s2))))

(define (head-sym<? a b)
  (and (pair? a)
       (pair? b)
       (symbol<? (car a) (car b))))

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

(define (block-data=? bd1 bd2)
  (and (sxml=? (car bd1) (car bd2))
       (alist=? (cadr bd1) (cadr bd2))))

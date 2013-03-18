;;; cav-templates.scm -- Implements a (somewhat) simple XML-based templating
;;;   system for the Coq au Vin blogging library.
;;;
;;;   Copyright © 2013 by Matthew C. Gushee <matt@gushee.net> 
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module cav-templates
        *
        (import scheme chicken)
        (import files)
        (import data-structures)
        (import extras)

        (use srfi-69)
        (use ssax)
        (use sxml-transforms)

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  GLOBAL DEFINITIONS  ----------------------------------------------

(define *site-path* (make-parameter #f))

(define *enable-l10n* (make-parameter #f))

(define *template-vars* (make-parameter (make-hash-table)))

(define *template-blocks* (make-parameter (make-hash-table)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  UTILITY FUNCTIONS  -----------------------------------------------

(define (template-path)
  (make-pathname (*site-path*) "templates"))
       
(define (alist-merge alist1 alist2)
  (let ((merge
          (lambda (out-list in-list)
            (let loop ((out out-list)
                       (in in-list))
              (if (null? in)
                out
                (let ((k (caar in))
                      (v (cdar in)))
                  (loop (alist-update k v out) (cdr in))))))))
    (merge (merge '() alist1) alist2)))
    
(define (get-var name)
  (hash-table-ref (*template-vars*) name))

(define (clear-template-vars)
  (hash-table-clear! (*template-vars*)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  XML PROCESSING  --------------------------------------------------

(define (read-template name #!key (ext "xhtml") (nsmap '()))
  (let* ((full-path (make-pathname (template-path) name ext))
         (input (open-input-file full-path))
         (default-nsmap
           '((#f . "http://www.w3.org/1999/xhtml")
             (cav . "http://xmlns.therebetygers.net/coq-au-vin/0.1")))
         (nsmap* (alist-merge default-nsmap nsmap)))
    (ssax:xml->sxml input nsmap*)))

; At the momemt this does nothing
(define (process-var value type format flags)
  value)

(define (parse-cav:attr attstring)
  (let ((split-attrs (lambda (s) (string-split s ";")))
        (split-name/varspec (lambda (s) (string-split s "=")))
        (split-varspec (lambda (s) (string-split s ":")))
        (split-params (lambda (s) (string-split s "/" #t)))
        (split-flags (lambda (s) (string-split s ","))))
    (let ((attrs (split-attrs attstring)))
      (let loop ((in attrs) (out '()))
        (if (null? in)
          out
          (let* ((attr (car in))
                 (n+v (split-name/varspec attr))
                 (name (car n+v))
                 (varspec (cadr n+v))
                 (var+params (split-varspec varspec))
                 (var (car var+params))
                 (params* (cdr var+params))
                 (params (and (not (null? params*)) (car params*)))
                 (t+f+f (and params (split-params params)))
                 (type* (and t+f+f (car t+f+f)))
                 (format* (and t+f+f (cadr t+f+f)))
                 (flags* (and t+f+f (split-flags (caddr t+f+f))))
                 (type (and type* (> (string-length type*) 0) (string->symbol type*)))
                 (format (and format* (> (string-length format*) 0) (string->symbol format*)))
                 (flags (and flags* (> (length flags*) 0) (map string->symbol flags*))))
            (let* ((value* (get-var var))
                   (value (process-var value* type format flags)))
              (loop (cdr in) (cons (list name value) out)))))))))

(define cav:var-rules
  `((name . ,(lambda (_ body) (get-var (car body))))))

(define template-rules
  `((cav:var ,cav:var-rules . ,(lambda (_ body) (cadar body)))
    (cav:attr . ,(lambda (_ body) (parse-cav:attr (car body))))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  TESTING  ---------------------------------------------------------

(define (var-test-setup vars)
  (let ((var-table (*template-vars*)))
    (for-each
      (lambda (elt)
        (let ((k (car elt)) (v (cdr elt)))
          (hash-table-set! var-table k v)))
      vars)))

(define (article-var-test)
  (clear-template-vars)
  (var-test-setup
    '(("urlScheme" . "HTTP") ("hostName" . "blog.therebetygers.net")
      ("articleID" . "4173208") ("bodyMD" . "lotsa Markdown!")
      ("canEdit" . "FALSE") ("seriesTitle" . "Mythical Creatures")
      ("partNo" . "23") ("articleTitle" . "The Snark Was a Boojum, You See")
      ("articleSubTitle" . "Or, How Toasted-Cheese Met His Match")
      ("articleBody" . "BODDYBODDY") ("copyrightYear" . "2011-2013")
      ("authorName" . "Margaret Snatcher")
      ("rightsStatement" . "You have no rights!")))
  (let ((doc (read-template "article" ext: "html"))
        (rules (append template-rules alist-conv-rules*)))
    (pp (pre-post-order* doc rules))))

(define (article-var+att-test)
  (clear-template-vars)
  (var-test-setup
    '(("urlScheme" . "HTTP") ("hostName" . "blog.therebetygers.net")
      ("articleID" . "4173208") ("bodyMD" . "lotsa Markdown!")
      ("canEdit" . "FALSE") ("seriesTitle" . "Mythical Creatures")
      ("partNo" . "23") ("articleTitle" . "The Snark Was a Boojum, You See")
      ("articleSubTitle" . "Or, How Toasted-Cheese Met His Match")
      ("articleBody" . "BODDYBODDY") ("copyrightYear" . "2011-2013")
      ("authorName" . "Margaret Snatcher")
      ("rightsStatement" . "You have no rights!")
      ("editURL" . "/path/for/editing") ("currentPath" . "/you/are/here")))
  (let ((doc (read-template "article" ext: "html"))
        (rules (append template-rules alist-conv-rules*)))
    (pp (pre-post-order* doc rules))))

(define (base-var-test)
  (clear-template-vars)
  (var-test-setup '(("htmlTitle" . "This is my template document.")))
  (let ((doc (read-template "base" ext: "html"))
        (rules (append template-rules alist-conv-rules*)))
    (pp (pre-post-order* doc rules))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------



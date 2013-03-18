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

        (use ssax)
        (use sxml-transforms)

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  GLOBAL DEFINITIONS  ----------------------------------------------

(define *site-path* (make-parameter #f))

(define *enable-l10n* (make-parameter #f))

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
    

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


(define (read-template name #!key (ext "xhtml") (nsmap '()))
  (let* ((full-path (make-pathname (template-path) name ext))
         (input (open-input-file full-path))
         (default-nsmap
           '((#f . "http://www.w3.org/1999/xhtml")
             (cav . "http://xmlns.therebetygers.net/coq-au-vin/0.1")))
         (nsmap* (alist-merge default-nsmap nsmap)))
    (ssax:xml->sxml input nsmap*)))

) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------



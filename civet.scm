;;; civet.scm -- Implements a (somewhat) simple XML-based templating
;;;   system for the Coq au Vin blogging library.
;;;
;;;   Copyright © 2013 by Matthew C. Gushee <matt@gushee.net> 
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module civet
        *
        (import scheme chicken)
        (import files)
        (import data-structures)
        (import extras)
        (import posix)
        (import utils)
        (import srfi-1)

        (use srfi-69)
        (use ssax)
        (use sxml-transforms)


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  GLOBAL DEFINITIONS  ----------------------------------------------

(define *site-path* (make-parameter #f))

(define *template-path* (make-parameter #f))

(define *template-cache-path* (make-parameter #f))

(define *enable-l10n* (make-parameter #f))

(define *template-vars* (make-parameter (make-hash-table)))

(define *template-blocks* (make-parameter (make-hash-table)))

(define civet-ns-uri "http://xmlns.therebetygers.net/civet/0.1")

(define civet-ns-prefix (make-parameter 'cvt))

(define (default-nsmap)
  `((#f . "http://www.w3.org/1999/xhtml")
    (,(cvt-ns-prefix) . ,cvt-ns-uri)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  UTILITY FUNCTIONS  -----------------------------------------------

(define (template-path)
  (or (*template-path*)
      (make-pathname (*site-path*) "templates")))

(define (template-cache-path)
  (or (*template-cache-path*)
      (make-pathname (template-path) ".cache")))
       
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
;;; ----  CONTEXT OBJECTS  -------------------------------------------------

(define (make-context #!optional (vars '())
                      #!key (nsmap (default-nsmap)) (attrs '()))
  (let ((parent-template #f)
        (blocks '()))
    (lambda (cmd . args)
      (case cmd
        ((get-var)
         (alist-ref (car args) vars))
        ((set-var!)
         (alist-update! (car args) (cadr args) vars))
        ((update-vars!)
         (set! vars (alist-merge vars args))) 
        ((set-vars!)
         (set! vars args))
        ((pfx->uri)
         (alist-ref (car args) nsmap))
        ((uri->pfx)
         (let ((pair (rassoc (car args) nsmap equal?)))
           (if pair (car pair) #:UNDEFINED)))
        ((set-ns!)
         (alist-update! (car args) (cadr args) vars))
        ((update-nsmap!)
         (set! nsmap (alist-merge nsmap args)))
        ((set-nsmap!)
         (set! nsmap args))
        ((get-attrs)
         attrs)
        ((set-attrs!)
         (set! attrs args))
        ((set-attr!)
         (alist-update! (car args) (cadr args) attrs))
        ((delete-attrs!)
         (set! attrs '()))
        ((set-parent-template!)
         (set! parent-template (car args)))
        ((get-parent-template)
         parent-template)
        ((get-block)
         (alist-ref (car args) blocks))
        ((set-block!)
         (alist-update! (car args) (cadr args) blocks)))))))


;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  XML PROCESSING  --------------------------------------------------

(define (update-cached-template? raw-path cached-path)
  (or (not (file-exists? raw-path))
      (not (file-exists? cached-path))
      (let ((raw-modtime (file-modification-time raw-path))
            (cached-modtime (file-modification-time cached-path)))
        (< cached-modtime raw-modtime))))

(define (load-template name #!key (ext "xhtml") (nsmap '()))
  (let* ((nsmap* (alist-merge (default-nsmap) nsmap))
         (raw-template (make-pathname (template-path) name ext))
         (cached-template (make-pathname (template-cache-path) name "sxml"))
         (update? (update-cached-template? raw-template cached-template))
         (infile (if update? raw-template cached-template))
         (input (open-input-file infile))
         (sxml
           (if update?
             (ssax:xml->sxml input nsmap*)
             (read input))))
    (close-input-port input)
    (when update?
      (with-output-to-file
        cached-template
        (lambda ()
          (write sxml))))
    sxml))


(define (resolve-block tpl ctx)

  (define (cvt-elt? tag #!optional (match-tag #f))
    (let ((parts (string-split (symbol->string tag) ":")))
      (and (= (length parts) 2)
           (string=? (ctx 'pfx->uri (string->symbol (car parts)))
                     civet-ns-uri)
           (or (not match-tag)
               (string=? (cadr parts) match-tag)))))

  (define (%cvt:var attrs content ctx) output)

  (define (%cvt:list attrs content ctx) output)

  (define (%cvt:object attrs content ctx) output)

  (define (%cvt:field attrs content ctx) output)

  (define (%cvt:if attrs content ctx) output)

  (define (%cvt:else attrs content ctx) output)

  (define (%cvt:each attrs content ctx) output)

  (define (%cvt:with attrs content ctx) output)

  (define (%cvt:defvar attrs content ctx) output)

  (define (%cvt:setattr attrs content ctx) output)

  (define (%cvt:attr attrs content ctx) output)

  (define (%cvt:@* name value ctx) output)

  (define (%cvt:* attrs content ctx) output)

  (define (%* attrs content ctx) output)

  (define (@* name value ctx) output)

  (define (process-tree tree ctx)
    (let ((head (car tree)))
      (if (list? head)
        (map
          (lambda (t) (process-tree t ctx))
          tree)
        (let ((attrs (get-attrs tree))
              (content (get-content tree)))
          (cond
            ((cvt-elt? head 'var) (%cvt:var attrs content ctx))
            ((cvt-elt? head 'list) (%cvt:list attrs content ctx))
            ((cvt-elt? head 'object) (%cvt:object attrs content ctx))
            ((cvt-elt? head 'field) (%cvt:field attrs content ctx))
            ((cvt-elt? head 'if) (%cvt:if attrs content ctx))
            ((cvt-elt? head 'else) (%cvt:else attrs content ctx))
            ((cvt-elt? head 'each) (%cvt:each attrs content ctx))
            ((cvt-elt? head 'with) (%cvt:with attrs content ctx))
            ((cvt-elt? head 'defvar) (%cvt:defvar attrs content ctx))
            ((cvt-elt? head 'setattr) (%cvt:setattr attrs content ctx))
            ((cvt-elt? head 'attr) (%cvt:attr attrs content ctx))
            (else tree)))))))

(define (resolve-template tpl ctx)

  (define (cvt-elt? tag #!optional (match-tag #f))
    (let ((parts (string-split (symbol->string tag) ":")))
      (and (= (length parts) 2)
           (string=? (ctx 'pfx->uri (string->symbol (car parts)))
                     civet-ns-uri)
           (or (not match-tag)
               (string=? (cadr parts) match-tag)))))

  (define (%cvt:template attrs content ctx) output)

  (define (%cvt:block attrs content ctx) output)

  (define (%cvt:super attrs content ctx) output)

  (define (%cvt:var attrs content ctx) output)

  (define (%cvt:list attrs content ctx) output)

  (define (%cvt:object attrs content ctx) output)

  (define (%cvt:field attrs content ctx) output)

  (define (%cvt:if attrs content ctx) output)

  (define (%cvt:else attrs content ctx) output)

  (define (%cvt:each attrs content ctx) output)

  (define (%cvt:with attrs content ctx) output)

  (define (%cvt:defvar attrs content ctx) output)

  (define (%cvt:setattr attrs content ctx) output)

  (define (%cvt:attr attrs content ctx) output)

  (define (%cvt:* attrs content ctx) output)

  (define (%* attrs content ctx) output)

  (define (@* name value ctx) output)

  (define (process-tree tree ctx)
    (let ((head (car tree)))
      (if (list? head)
        (map process-tree tree)
        (let ((attrs (get-attrs tree))
              (content (get-content tree)))
          (cond
            ((cvt-elt? head 'template) (%cvt:template attrs content ctx))
            ((cvt-elt? head 'block) (%cvt:block attrs content ctx))
            ((cvt-elt? head 'super) (%cvt:super attrs content ctx))
            ((cvt-elt? head 'var) (%cvt:var attrs content ctx))
            ((cvt-elt? head 'list) (%cvt:list attrs content ctx))
            ((cvt-elt? head 'object) (%cvt:object attrs content ctx))
            ((cvt-elt? head 'field) (%cvt:field attrs content ctx))
            ((cvt-elt? head 'if) (%cvt:if attrs content ctx))
            ((cvt-elt? head 'else) (%cvt:else attrs content ctx))
            ((cvt-elt? head 'each) (%cvt:each attrs content ctx))
            ((cvt-elt? head 'with) (%cvt:with attrs content ctx))
            ((cvt-elt? head 'defvar) (%cvt:defvar attrs content ctx))
            ((cvt-elt? head 'setattr) (%cvt:setattr attrs content ctx))
            ((cvt-elt? head 'attr) (%cvt:attr attrs content ctx))
            (else #f)))))))


;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  TESTING  ---------------------------------------------------------

; (define (var-test-setup vars)
;   (let ((var-table (*template-vars*)))
;     (for-each
;       (lambda (elt)
;         (let ((k (car elt)) (v (cdr elt)))
;           (hash-table-set! var-table k v)))
;       vars)))
; 
; (define (article-var-test)
;   (clear-template-vars)
;   (var-test-setup
;     '(("urlScheme" . "HTTP") ("hostName" . "blog.therebetygers.net")
;       ("articleID" . "4173208") ("bodyMD" . "lotsa Markdown!")
;       ("canEdit" . "FALSE") ("seriesTitle" . "Mythical Creatures")
;       ("partNo" . "23") ("articleTitle" . "The Snark Was a Boojum, You See")
;       ("articleSubTitle" . "Or, How Toasted-Cheese Met His Match")
;       ("articleBody" . "BODDYBODDY") ("copyrightYear" . "2011-2013")
;       ("authorName" . "Margaret Snatcher")
;       ("rightsStatement" . "You have no rights!")))
;   (let ((doc (load-template "article" ext: "html"))
;         (rules (append template-rules alist-conv-rules*)))
;     (pp (pre-post-order* doc rules))))
; 
; (define (article-var+att-test)
;   (clear-template-vars)
;   (var-test-setup
;     '(("urlScheme" . "HTTP") ("hostName" . "blog.therebetygers.net")
;       ("articleID" . "4173208") ("bodyMD" . "lotsa Markdown!")
;       ("canEdit" . "FALSE") ("seriesTitle" . "Mythical Creatures")
;       ("partNo" . "23") ("articleTitle" . "The Snark Was a Boojum, You See")
;       ("articleSubTitle" . "Or, How Toasted-Cheese Met His Match")
;       ("articleBody" . "BODDYBODDY") ("copyrightYear" . "2011-2013")
;       ("authorName" . "Margaret Snatcher")
;       ("rightsStatement" . "You have no rights!")
;       ("editURL" . "/path/for/editing") ("currentPath" . "/you/are/here")))
;   (let ((doc (load-template "article" ext: "html"))
;         (rules (append template-rules alist-conv-rules*)))
;     (pp (pre-post-order* doc rules))))
; 
; (define (base-var-test)
;   (clear-template-vars)
;   (var-test-setup '(("htmlTitle" . "This is my template document.")))
;   (let ((doc (load-template "base" ext: "html"))
;         (rules (append template-rules alist-conv-rules*)))
;     (pp (pre-post-order* doc rules))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------



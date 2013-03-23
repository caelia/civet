;;; civet.scm -- Implements a (somewhat) simple XML-based templating
;;;   system for the Coq au Vin blogging library.
;;;
;;;   Copyright © 2013 by Matthew C. Gushee <matt@gushee.net> 
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module civet
        *
        (import scheme chicken)
        (import
          (except scheme
            string-length string-ref string-set! make-string string substring
            string->list list->string string-fill! write-char read-char display)
          (except chicken
            reverse-list->string print print*)
          (except data-structures
            ->string conc string-chop string-split string-translate
            substring=? substring-ci=? substring-index substring-index-ci)
          (except extras
            read-string write-string read-token))
        (import files)
        (import posix)
        (import utils)
        (import srfi-1)

        (use utf8)
        (use utf8-srfi-13)
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

; (define civet-ns-uri "http://xmlns.therebetygers.net/civet/0.1")

; (define civet-ns-prefix (make-parameter 'cvt))
 
(define *civet-ns-prefix* (make-parameter 'cvt))
 
(define *civet-ns-uri* (make-parameter "http://xmlns.therebetygers.net/civet/0.1"))

; (define (default-nsmap)
;   `((#f . "http://www.w3.org/1999/xhtml")
;     (,(cvt-ns-prefix) . ,cvt-ns-uri)))
; 
; (define *default-nsmap*
;   (make-parameter
;     '((#f . "http://www.w3.org/1999/xhtml")
;       ((cvt . "http://xmlns.therebetygers.net/civet/0.1")))))

(define *default-nsmap*
  (make-parameter
    `((#f . "http://www.w3.org/1999/xhtml")
      (,(*civet-ns-prefix*) . ,(*civet-ns-uri*)))))

(define *sxpath-nsmap*
  (make-parameter
    (let ((default-map (*default-nsmap*)))
      (cons
        (cons '*default* (cdar default-map))
        (cdr default-map)))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  UTILITY FUNCTIONS  -----------------------------------------------

(define (eprintf fmt . args)
  (error (apply sprintf (cons fmt args))))

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
;;; ----  VARIABLE TYPES  --------------------------------------------------

(define (string->bool s)
  (let ((s (string-downcase s)))
    (or (string=? s "t")
        (string=? s "true")
        (string=? s "1"))))

(define (bool->string b #!optional (format 'tc))
  (if b
    (case format
      ((ts) "True")
      ((us) "TRUE")
      ((ls) "true")
      ((uc) "T")
      ((lc) "t")
      ((no) "1")
      (else (eprintf "Unrecognized format symbol '~A" format)))
    (case format
      ((ts) "False")
      ((us) "FALSE")
      ((ls) "false")
      ((uc) "F")
      ((lc) "f")
      ((no) "0")
      (else (eprintf "Unrecognized format symbol '~A" format)))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  CONTEXT OBJECTS  -------------------------------------------------

(define (make-context #!optional (vars '())
                      #!key (nsmap (*default-nsmap*)) (attrs '()))
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
        ((get-vars)
         vars)
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
         (alist-update! (car args) (cadr args) blocks))))))


;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  TEMPLATE SETS  ---------------------------------------------------

(define (make-block+context block vars locale)
  (lambda (cmd)
    (case cmd
      ((block) block)
      ((vars) vars)
      ((locale) locale))))

(define (make-template-set
          #!key (templates '()) (vars '()) (blocks '()) (block-index '()))
  (lambda (cmd . args)
    (case cmd
      ((add-template-vars)
       (alist-update! (car args) (cadr args) template-vars))
      ((add-template-locale)
       (alist-update! (car args) (cadr args) template-locales))
      ((add-block)
       (alist-update! (car args) (cadr args) blocks))
      ((get-block)
       (alist-ref (car args) vars)))))


(define (update-cached-template? raw-path cached-path)
  (or (not (file-exists? raw-path))
      (not (file-exists? cached-path))
      (let ((raw-modtime (file-modification-time raw-path))
            (cached-modtime (file-modification-time cached-path)))
        (< cached-modtime raw-modtime))))

(define (load-template name #!optional (nsmap '()))
  (let* ((nsmap* (alist-merge (*default-nsmap*) nsmap))
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

(define (extension? template)
  (let ((sp (sxpath '(cvt:template) (*sxpath-nsmap*))))
    (not (null? (sp template)))))

(define (get-parent-name template)
  (let ((sp
          (sxpath
            '(cvt:template @ extends *text*)
            (*sxpath-nsmap*))))
    (sp template)))

(define (get-template-locale template)
  (let* ((sp (sxpath '(cvt:template cvt:locale @ *any*) (*sxpath-nsmap*)))
         (locale-data (sp template)))
    (map
      (lambda (elt) (cons (car elt) (cadr elt)))
      locale-data)))

(define (get-template-vars template)
  (let ((sp1 (sxpath '(cvt:template cvt:defvar) (*sxpath-nsmap*)))
        (sp2 (sxpath '(@ name *text*))))
    (map
      (lambda (def)
        (let* ((name* (sp2 def))
               (name (string->symbol (car name))))
          (cons name def)))
      (sp1 template))))

(define (build-template-set name #!optional (nsmap '()))
  (let ((sp1 (sxpath '(cvt:template *)))
        (sp2 (sxpath '(@ name *text*))))
    (let loop ((template (load-template name nsmap))
               (blocks '()))
      (if (extension? template)
        (let ((parent (get-parent-name template)))
          (when (null? parent)
            (eprintf "Parent template '~A' not found.\n" parent))
          (let ((locale (get-template-locale template))
                (vars (get-template-vars template))
                (kids (sp1 template)))
            (loop
              (load-template parent nsmap)
              (foldl
                (lambda (k)
                  (if (eqv? (car k) 'cvt:block)
                    (let* ((name* (sp2 k))
                           (name (string->symbol (car name*))))
                      (if (alist-ref name blocks)
                        blocks
                        (cons (cons name (list locale vars k)) blocks)))
                    blocks))
                blocks kids))))
        (values blocks template)))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  XML PROCESSING  --------------------------------------------------

(define (get-attrs attlist)
  (let ((se '(sxpath (@))))
    (se attlist)))

; input is the entire attributes node: '(@ ((name value) ...))`
(define (get-attval attlist name)
  (let ((se '(sxpath (name *text*))))
    (se attlist)))

(define (get-kids node #!optional (nsmap #f))
  (let* ((default-nsmap (*sxpath-nsmap*))
         (nsmap
           (if nsmap
             (alist-merge default-nsmap nsmap)
             default-nsmap))
         (xp (sxpath '(*any*) nsmap)))
    (xp node)))

(define (except-attlist node #!optional (nsmap #f))
  (let ((kids (get-kids node nsmap)))
    (filter
      (lambda (node)
        (or (not (list? node))
            (not (eqv? (car node) '@))))
      kids)))

(define (cvt-elt? tag #!optional (match-tag #f))
  (let ((parts (string-split (symbol->string tag) ":")))
    (and (= (length parts) 2)
         (string=? (ctx 'pfx->uri (string->symbol (car parts)))
                   (*civet-ns-uri*))
         (or (not match-tag)
             (string=? (cadr parts) match-tag)))))

(define (%cvt:block attrs content ctx) #f)

(define (%cvt:var attrs content ctx) #f)

(define (%cvt:list attrs content ctx) #f)

(define (%cvt:object attrs content ctx) #f)

(define (%cvt:field attrs content ctx) #f)

(define (%cvt:if attrs content ctx) #f)

(define (%cvt:else attrs content ctx) #f)

(define (%cvt:each attrs content ctx) #f)

(define (%cvt:with attrs content ctx) #f)

(define (%cvt:defvar attrs content ctx) #f)

(define (%cvt:attr attrs content ctx) #f)

(define (%cvt:* attrs content ctx) #f)

(define (%* attrs content ctx) #f)

(define (@* name value ctx) #f)


(define (process-block block-data ctx)

  (define (cvt-elt? tag #!optional (match-tag #f))
    (let ((parts (string-split (symbol->string tag) ":")))
      (and (= (length parts) 2)
           (string=? (ctx 'pfx->uri (string->symbol (car parts)))
                     (*civet-ns-uri*))
           (or (not match-tag)
               (string=? (cadr parts) match-tag)))))

  (define (%cvt:var attrs content ctx)
    (let* ((var-name (get-attval attrs "name"))
           (value (ctx 'get-var var-name))
           (req-str (get-attval attrs "required"))
           (required (or (not req-str)
                         (string->bool req-str))))
      (cond
        ((and required (not value))
         (eprintf "No value provided for required variable '~A'\n." var-name))
        ((value) value)
        (else '()))))

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
            ((cvt-elt? head 'object) (%cvt:object attrs content ctx))
            ((cvt-elt? head 'field) (%cvt:field attrs content ctx))
            ((cvt-elt? head 'if) (%cvt:if attrs content ctx))
            ((cvt-elt? head 'else) (%cvt:else attrs content ctx))
            ((cvt-elt? head 'each) (%cvt:each attrs content ctx))
            ((cvt-elt? head 'with) (%cvt:with attrs content ctx))
            ((cvt-elt? head 'defvar) (%cvt:defvar attrs content ctx))
            ((cvt-elt? head 'attr) (%cvt:attr attrs content ctx))
            (else tree)))))))

(define (process-template tpl ctx)

  (define (process-tree tree ctx)
    (let ((head (car tree)))
      (if (list? head)
        (map process-tree tree)
        (let ((attrs (get-attrs tree))
              (content (get-content tree)))
          (cond
            ((cvt-elt? head 'template) (%cvt:template attrs content ctx))
            ((cvt-elt? head 'block) (%cvt:block attrs content ctx))
            ((cvt-elt? head 'var) (%cvt:var attrs content ctx))
            ((cvt-elt? head 'object) (%cvt:object attrs content ctx))
            ((cvt-elt? head 'field) (%cvt:field attrs content ctx))
            ((cvt-elt? head 'if) (%cvt:if attrs content ctx))
            ((cvt-elt? head 'else) (%cvt:else attrs content ctx))
            ((cvt-elt? head 'each) (%cvt:each attrs content ctx))
            ((cvt-elt? head 'with) (%cvt:with attrs content ctx))
            ((cvt-elt? head 'defvar) (%cvt:defvar attrs content ctx))
            ((cvt-elt? head 'attr) (%cvt:attr attrs content ctx))
            (else #f)))))))


(define (render template-name context #!key (port #f) (file #f) (nsmap '()))
  (let-values (((block-data template)
                (build-template-set template-name nsmap)))
    (let ((final-tree (process-template template block-data context nsmap)))
      (serialize-sxml final-tree output: (or port file) ns-prefixes: (*sxpath-nsmap*)))))


;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------



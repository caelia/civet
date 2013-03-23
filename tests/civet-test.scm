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



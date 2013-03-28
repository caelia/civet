(use civet)
(use test)

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  TEMPLATE FILE HANDLING  ------------------------------------------

;;; ------  Support data for tests  ----------------------------------------

(define doc1-raw-xml
#<<XML
<a>
  <b>
    <c>Raw version of the template.</c>
  </b>
  <b>
    <c>Some other text.</c>
  </b>
</a>
XML
)

(define doc1-raw-sxml
  '(*TOP*
     (a
       (b
         (c "Raw version of the template."))
       (b
         (c "Some other text.")))))

(define doc1-cached-sxml
  '(*TOP*
     (a
       (b
         (c "Cached version of the template."))
       (b
         (c "Some other text.")))))

;;; ========================================================================
;;; ------  Run tests  -----------------------------------------------------

(test-group "Template Files"
  (test
    "Cached SXML file is newer than raw XML--should load cached version." 
    doc1-cached-sxml
    (begin
      (with-output-to-file "templates/doc1.xml" (lambda () (display doc1-raw-xml)))
      (with-output-to-file "templates/.cache/doc1.sxml" (lambda () (write doc1-cached-sxml)))
      (load-template "doc1.xml")))
  (test
    "Raw XML file is newer than cached SXML--should load raw version." 
    doc1-raw-sxml
    (begin
      ;; Kill the namespace map
      (*default-nsmap* '())
      (sleep 1)
      (with-output-to-file "templates/doc1.xml" (lambda () (display doc1-raw-xml)))
      (load-template "doc1.xml")))
  (test
    "Cache file should have been updated in previous step."
    doc1-raw-sxml
    (with-input-from-file "templates/.cache/doc1.sxml" (lambda () (read)))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  UTILITY FUNCTIONS  -----------------------------------------------


;;; ------------------------------------------------------------------------

;;; ========================================================================
;;; ------  Run tests  -----------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  CONTEXT OBJECTS  -------------------------------------------------

(define ctx01
  (make-context
    vars: '((aardvark . 219) (behemoth . 219.0) (codex . "Silly Putty")
            (darwinism . #f) (elation . '((a . "ay") (b . "bee") (c . "see")))
            (finch . 219) (gastropod . 324) (helium . 323.999)
            (irate . 324.0) (jaguar . #t) (keratin . "Keratin!")
            (louise . #\L) (metanoia . '(3 7 10 9 12 4 13))
            (narwhal . '("alabaster" "brittle" "codicils" "dervish"))
            (oxymoron . '("alabaster" "brittle" "codicils" "dervish"))
            (pernicious . '("dervish" "brittle" "codicils" "alabaster"))
            (quixotic . #t) (rapacious . (vector #\x #\y #\z))
            (serpent . "That's all!"))
    state: 'init))

;;; ========================================================================
;;; ------  Run tests  -----------------------------------------------------

(test-group "Context Objects"
  (test "Retrieve a variable" #\L (ctx01 'get-var 'louise)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  EXPRESSION LANGUAGE  ---------------------------------------------

(test-group "Expression Language"
  (test "Integer variable = integer constant  [#t]" #t (eval-test "aardvark = 219" ctx01))
  (test "Integer variable = float constant    [#t]" #t (eval-test "aardvark = 219.0" ctx01))
  (test "Integer variable = integer variable  [#t]" #t (eval-test "aardvark = finch" ctx01))
  (test "Integer variable = float variable    [#t]" #t (eval-test "aardvark = behemoth" ctx01))
  (test "Integer variable != integer constant [#t]" #t (eval-test "aardvark != 500" ctx01))
  (test "Integer variable != float constant   [#t]" #t (eval-test "aardvark != 219.1" ctx01))
  (test "Integer variable != integer variable [#t]" #t (eval-test "aardvark != gastropod" ctx01))
  (test "Integer variable != float variable   [#t]" #t (eval-test "gastropod != helium" ctx01)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  BASIC SXML PROCESSING  -------------------------------------------


;;; ------------------------------------------------------------------------

;;; ========================================================================
;;; ------  Run tests  -----------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  SIMPLE TEMPLATE PROCESSING  --------------------------------------
;;; ------  Support data  --------------------------------------------------

(define min-ctx
  (make-context state: 'init))

(define min-doc
  '(*TOP*
     (a
       (b "Some text."))))

(define ctx-tr1-1
  (make-context state: 'init
                vars: '((color . "green"))
                nsmap: '((#f . "http://www.w3.org/1999/xhtml")
                         (cvt . "http://xmlns.therebetygers.net/civet/0.1"))))

(define doc-tr1-1
  '(*TOP*
     (@
       (*NAMESPACES* 
         (#f "http://www.w3.org/1999/xhtml")
         (cvt "http://xmlns.therebetygers.net/civet/0.1")))
     (html
       (head
         (title "test doc"))
       (body
         (p "Pointless text")))))

(define doc-tr1-2-in
  '(*TOP*
     (@
       (*NAMESPACES* 
         (#f "http://www.w3.org/1999/xhtml")
         (cvt "http://xmlns.therebetygers.net/civet/0.1")))
     (html
       (head
         (title "test doc"))
       (body
         (p
           (cvt:var (@ (name "color"))))))))

(define doc-tr1-2-out
  '(*TOP*
     (@
       (*NAMESPACES* 
         (#f "http://www.w3.org/1999/xhtml")
         (cvt "http://xmlns.therebetygers.net/civet/0.1")))
     (html
       (head
         (title "test doc"))
       (body
         (p "green")))))

;;; ========================================================================
;;; ------  Run tests  -----------------------------------------------------

(test-group "[TR1] Simple Transformations"
  (test
    "TR1.01: Simple Template without CVT Markup (identity)"
    min-doc
    (process-base-template min-doc '() min-ctx))
  (test
    "TR1.02: XHTML Template without CVT Markup (identity)"
    doc-tr1-1
    (process-base-template doc-tr1-1 '() ctx-tr1-1))
  (test
    "TR1.03: Basic string variable substitution"
    doc-tr1-2-out
    (process-base-template doc-tr1-2-in '() ctx-tr1-1)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  TEMPLATE SET CONSTRUCTION  ---------------------------------------
;;; ------  Support data  --------------------------------------------------

(define ctx-tr2-1
  (make-context state: 'init
                nsmap: '((#f . "http://www.w3.org/1999/xhtml")
                         (cvt . "http://xmlns.therebetygers.net/civet/0.1"))))

(define doc-tr2-1
  '(*TOP*
     (@
       (*NAMESPACES* 
         (#f "http://www.w3.org/1999/xhtml")
         (cvt "http://xmlns.therebetygers.net/civet/0.1")))
     (cvt:template
       (html
         (head
           (title "Bad template"))
         (body
           (p "Should never see the light of day"))))))

(define doc-tr2-3
  '(*TOP*
     (@
       (*NAMESPACES* 
         (#f "http://www.w3.org/1999/xhtml")
         (cvt "http://xmlns.therebetygers.net/civet/0.1")))
     (html
       (cvt:template
         (@
           (extends "fubar.html"))
         (head
           (title "Bad template"))
         (body
           (p "Should never see the light of day"))))))

;;; ========================================================================
;;; ------  Run tests  -----------------------------------------------------

(test-group "[TR2] Template Sets"
  (test-error
    "TR2.01: cvt:template with no extension attribute [ERROR]"
    (process-base-template doc-tr2-1 '() ctx-tr2-1))
  (test-error
    "TR2.02: cvt:template as descendant of the document element [ERROR]"
    (process-base-template doc-tr2-1 '() ctx-tr2-1)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  COMPLEX TEMPLATE PROCESSING  -------------------------------------


;;; ------------------------------------------------------------------------

;;; ========================================================================
;;; ------  Run tests  -----------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

(test-exit)

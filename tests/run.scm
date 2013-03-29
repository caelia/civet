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
  (test "Retrieve a variable" #\L (ctx01 'get-var 'louise))
  (test "Check the current state" 'init (ctx01 'get-state)))

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

(define ctx-tr1-3
  (make-context state: 'init
                nsmap: '((#f . "http://www.w3.org/1999/xhtml")
                         (cvt . "http://xmlns.therebetygers.net/civet/0.1"))
                vars:  '((color . "lime") (size . "12") (age . "27")
                         (divclass . "Poinsettia") (divid . "baz451") (chapeau . "porkpie"))))

(define doc-tr1-3-in
'(*TOP*
   (@ (*NAMESPACES*
        (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (cvt:if
             (@ (test "color"))
             "12")))))

(define doc-tr1-3-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body "12"))))

(define doc-tr1-4-in
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (cvt:if
             (@ (test "bolour"))
             "12"
             (cvt:else "27"))))))

(define doc-tr1-4-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body "27"))))

(define doc-tr1-5-in
'(*TOP*
   (@ (*NAMESPACES*
        (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (cvt:if
             (@ (test "color"))
             (cvt:var (@ (name "size"))))))))

(define doc-tr1-5-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body "12"))))

(define doc-tr1-6-in
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (cvt:if
             (@ (test "bolour"))
             (cvt:var (@ (name "size")))
             (cvt:else
               (cvt:var (@ (name "age")))))))))

(define doc-tr1-6-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body "27"))))

(define doc-tr1-7-in
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (cvt:if
             (@ (test "color"))
             (p (cvt:var (@ (name "size")))))))))

(define doc-tr1-7-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (p "12")))))

(define doc-tr1-8-in
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (cvt:if
             (@ (test "color"))
             (p (cvt:var (@ (name "size"))))
             (cvt:else
               (div (@ (id "age-div") (class "info"))
                    (cvt:var (@ (name "age"))))))))))

(define doc-tr1-8-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (p "12")))))

(define doc-tr1-9-in
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body (div (@ (cvt:class "divclass")) "Div Contents")))))

(define doc-tr1-9-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body (div (@ (class "Poinsettia")) "Div Contents")))))

(define doc-tr1-10-in
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (div
             (@ (id "fubar79") (class "Poinsettia"))
             (cvt:attr (@ (name "id"))
                       (cvt:var (@ (name "divid"))))
             (cvt:attr (@ (value "fedora") (name "chapeau")))
             "Div Contents")))))

(define doc-tr1-10-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (div
             (@ (class "Poinsettia") (id "baz451") (chapeau "fedora"))
             "Div Contents")))))

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
    (process-base-template doc-tr1-2-in '() ctx-tr1-1))
  (test
    "TR1.04: cvt:if, no else clause, literal string"
    doc-tr1-3-out
    (process-base-template doc-tr1-3-in '() ctx-tr1-3))
  (test
    "TR1.05: cvt:if + else clause, literal string"
    doc-tr1-4-out
    (process-base-template doc-tr1-4-in '() ctx-tr1-3))
  (test
    "TR1.06: cvt:if, no else clause, string variable only"
    doc-tr1-5-out
    (process-base-template doc-tr1-5-in '() ctx-tr1-3))
  (test
    "TR1.07: cvt:if + else clause, string variable only"
    doc-tr1-6-out
    (process-base-template doc-tr1-6-in '() ctx-tr1-3))
  (test
    "TR1.08: cvt:if, no else clause, string var in literal element"
    doc-tr1-7-out
    (process-base-template doc-tr1-7-in '() ctx-tr1-3))
  (test
    "TR1.09: cvt:if + else clause, string var in literal element"
    doc-tr1-8-out
    (process-base-template doc-tr1-8-in '() ctx-tr1-3))
  (test
    "TR1.10: direct attribute substitution"
    doc-tr1-9-out
    (process-base-template doc-tr1-9-in '() ctx-tr1-3))
  (test
    "TR1.11: attribute substitution using cvt:attr elements"
    doc-tr1-10-out
    (process-base-template doc-tr1-10-in '() ctx-tr1-3)))

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

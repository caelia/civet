(use civet)
(use test)
(use posix files)
(include "test-support.scm")

(create-directory (make-pathname "templates" ".cache") #t)

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
      (sleep 1)
      (with-output-to-file "templates/doc1.xml" (lambda () (display doc1-raw-xml)))
      (load-template "doc1.xml" #f)))
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

(define ctx-tr1-4
  (make-context state: 'init
                nsmap: '((#f . "http://www.w3.org/1999/xhtml")
                         (cvt . "http://xmlns.therebetygers.net/civet/0.1"))
                vars:  '((color . "lime") (size . "12") (age . "27")
                         (divclass . "Poinsettia") (divid . "baz451") (chapeau . "porkpie"))))

(define doc-tr1-4-in
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

(define doc-tr1-4-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body "12"))))

(define doc-tr1-5-in
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (cvt:if
             (@ (test "bolour"))
             "12"
             (cvt:else "27"))))))

(define doc-tr1-5-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body "27"))))

(define doc-tr1-6-in
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

(define doc-tr1-6-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body "12"))))

(define doc-tr1-7-in
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

(define doc-tr1-7-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body "27"))))

(define doc-tr1-8-in
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (cvt:if
             (@ (test "color"))
             (p (cvt:var (@ (name "size")))))))))

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
         (body
           (cvt:if
             (@ (test "bolour"))
             (p (cvt:var (@ (name "size"))))
             (cvt:else
               (div (@ (id "age-div") (class "info"))
                    (cvt:var (@ (name "age"))))))))))

(define doc-tr1-9-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (div (@ (id "age-div") (class "info"))
                "27")))))

(define doc-tr1-10-in
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (cvt:if
             (@ (test "color"))
             (p (cvt:var (@ (name "size"))))
             (p (cvt:var (@ (name "age")))))))))

(define doc-tr1-10-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (p "12")
           (p "27")))))

(define doc-tr1-11-in
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body (div (@ (cvt:class "divclass")) "Div Contents")))))

(define doc-tr1-11-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body (div (@ (class "Poinsettia")) "Div Contents")))))

(define doc-tr1-12-in
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

(define doc-tr1-12-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (div
             (@ (class "Poinsettia") (id "baz451") (chapeau "fedora"))
             "Div Contents")))))

(define ctx-tr1-13
  (make-context state: 'init
                nsmap: '((#f . "http://www.w3.org/1999/xhtml")
                         (cvt . "http://xmlns.therebetygers.net/civet/0.1"))
                vars:  '((lizards . ("gecko" "komodo dragon" "gila monster" "chameleon"))
                         (books . ("Frankenstein" "The Wise Man's Fear" "The Left Hand of Darkness"
                                   "One Hundred Years of Solitude" "Disaster Capitalism"))
                         (libro . ((title . "A Study in Scarlet")
                                   (author . "Sir Arthur Conan Doyle")
                                   (desc . "In which we encounter the extraordinary &amp; eccentric Sherlock Holmes for the first time.")))
                         (livres . (((title . "Frankenstein")
                                     (author . "Mary Shelley")
                                     (desc . "The seminal tale of technological hubris."))
                                    ((title . "The Wise Man's Fear")
                                     (author . "Patrick Rothfuss")
                                     (desc . "The second volume in the story of Kvothe, a world-destroying wizard."))
                                    ((title . "The Left Hand of Darkness")
                                     (author . "Ursula K. LeGuin")
                                     (desc . "An interplanetary diplomat has an eye-opening encounter with a humanoid race whose people switch gender according to the situation."))
                                    ((title . "The War of the End of the World")
                                     (author . "Mario Vargas Llosa")
                                     (desc . "A band of outcasts try to build a utopian community deep in the Amazon jungle, only to be slaughtered by the Brazilian army."))
                                    ((title .  "Disaster Capitalism")
                                     (author . "Naomi Klein")
                                     (desc . "The author explains how the global elite take advantage of natural and political crises to advance the neoliberal economic agenda."))))
                         (ein-buch . (((title . "A Study in Scarlet")
                                       (author . "Sir Arthur Conan Doyle")
                                       (desc . "In which we encounter the extraordinary &amp; eccentric Sherlock Holmes for the first time.")))))))


(define doc-tr1-13-in
'(*TOP*
  (@
    (*NAMESPACES*
      (#f "http://www.w3.org/1999/xhtml")
      (cvt "http://xmlns.therebetygers.net/civet/0.1")))
  (html
    (@ (xml:lang "en") (lang "en"))
   
    (head (title))
      (body 
        (cvt:for
          (@ (in "lizards") (each "x"))
          (cvt:var
            (@ (name "x")))
          (br))))))

(define doc-tr1-13-out
'(*TOP*
  (@
    (*NAMESPACES*
      (#f "http://www.w3.org/1999/xhtml")
      (cvt "http://xmlns.therebetygers.net/civet/0.1")))
  (html
    (@ (xml:lang "en") (lang "en"))
    (head (title))
      (body
        "chameleon"
        (br)
        "gecko"
        (br)
        "gila monster"
        (br)
        "komodo dragon"
        (br)))))

(define doc-tr1-14-in
'(*TOP*
  (@
    (*NAMESPACES*
      (#f "http://www.w3.org/1999/xhtml")
      (cvt "http://xmlns.therebetygers.net/civet/0.1")))
  (html
    (@ (xml:lang "en") (lang "en"))
    (head (title))
      (body 
        (cvt:for
          (@ (in "books") (each "book") (sort "none"))
          (p
            (cvt:var
              (@ (name "book")))))))))

(define doc-tr1-14-out
'(*TOP*
  (@
    (*NAMESPACES*
      (#f "http://www.w3.org/1999/xhtml")
      (cvt "http://xmlns.therebetygers.net/civet/0.1")))
  (html
    (@ (xml:lang "en") (lang "en"))
    (head (title))
      (body 
        (p "Frankenstein")
        (p "The Wise Man's Fear")
        (p "The Left Hand of Darkness")
        (p "One Hundred Years of Solitude")
        (p "Disaster Capitalism")))))

(define doc-tr1-15-in
'(*TOP*
  (@
    (*NAMESPACES*
      (#f "http://www.w3.org/1999/xhtml")
      (cvt "http://xmlns.therebetygers.net/civet/0.1")))
  (html
    (@ (xml:lang "en") (lang "en"))
    (head (title))
      (body 
        (div
          (@ (class "bookinfo"))
          (table
            (tr
              (th "Title")
              (td
                (cvt:var
                  (@ (name "libro.title")))))
            (tr
              (th "Author")
              (td
                (cvt:var
                  (@ (name "libro.author")))))
            (tr
              (th "Description")
              (td
                (cvt:var
                  (@ (name "libro.desc")))))))))))

(define doc-tr1-15-out
'(*TOP*
  (@
    (*NAMESPACES*
      (#f "http://www.w3.org/1999/xhtml")
      (cvt "http://xmlns.therebetygers.net/civet/0.1")))
  (html
    (@ (xml:lang "en") (lang "en"))
    (head (title))
      (body 
        (div
          (@ (class "bookinfo"))
          (table
            (tr
              (th "Title")
              (td "A Study in Scarlet"))
            (tr
              (th "Author")
              (td "Sir Arthur Conan Doyle"))
            (tr
              (th "Description")
              (td "In which we encounter the extraordinary &amp; eccentric Sherlock Holmes for the first time."))))))))

(define doc-tr1-16-in
'(*TOP*
  (@
    (*NAMESPACES*
      (#f "http://www.w3.org/1999/xhtml")
      (cvt "http://xmlns.therebetygers.net/civet/0.1")))
  (html
    (@ (xml:lang "en") (lang "en"))
    (head (title))
      (body 
        (cvt:for
          (@ (in "livres") (each "livre") (sort-field "title"))
          (div
            (@ (class "bookinfo"))
            (table
              (tr
                (th "Title")
                (td (cvt:var (@ (name "livre.title")))))
              (tr
                (th "Author")
                (td (cvt:var (@ (name "livre.author")))))
              (tr
                (th "Description")
                (td (cvt:var (@ (name "livre.desc"))))))))))))

(define doc-tr1-16-out
'(*TOP*
  (@
    (*NAMESPACES*
      (#f "http://www.w3.org/1999/xhtml")
      (cvt "http://xmlns.therebetygers.net/civet/0.1")))
  (html
    (@ (xml:lang "en") (lang "en"))
    (head (title))
      (body 
        (div (@ (class "bookinfo"))
          (table
            (tr
              (th "Title")
              (td "Disaster Capitalism"))
            (tr
              (th "Author")
              (td "Naomi Klein"))
            (tr
              (th "Description")
              (td "The author explains how the global elite take advantage of natural and political crises to advance the neoliberal economic agenda."))))
        (div
          (@ (class "bookinfo"))
          (table
            (tr
              (th "Title")
              (td "Frankenstein"))
            (tr
              (th "Author")
              (td "Mary Shelley"))
            (tr
              (th "Description")
              (td "The seminal tale of technological hubris."))))
        (div (@ (class "bookinfo"))
          (table
            (tr
              (th "Title")
              (td "The Left Hand of Darkness"))
            (tr
              (th "Author")
              (td "Ursula K. LeGuin"))
            (tr
              (th "Description")
              (td "An interplanetary diplomat has an eye-opening encounter with a humanoid race whose people switch gender according to the situation."))))
        (div (@ (class "bookinfo"))
          (table
            (tr
              (th "Title")
              (td "The War of the End of the World"))
            (tr
              (th "Author")
              (td "Mario Vargas Llosa"))
            (tr
              (th "Description")
              (td "A band of outcasts try to build a utopian community deep in the Amazon jungle, only to be slaughtered by the Brazilian army."))))
        (div (@ (class "bookinfo"))
          (table
            (tr
              (th "Title")
              (td "The Wise Man's Fear"))
            (tr
              (th "Author")
              (td "Patrick Rothfuss"))
            (tr
              (th "Description")
              (td "The second volume in the story of Kvothe, a world-destroying wizard."))))))))

(define doc-tr1-17-in
'(*TOP*
  (@
    (*NAMESPACES*
      (#f "http://www.w3.org/1999/xhtml")
      (cvt "http://xmlns.therebetygers.net/civet/0.1")))
  (html
    (@ (xml:lang "en") (lang "en"))
    (head (title))
      (body 
        (cvt:for
          (@ (in "ein-buch") (each "buch") (sort-field "title"))
          (div
            (@ (class "bookinfo"))
            (table
              (tr
                (th "Title")
                (td (cvt:var (@ (name "buch.title")))))
              (tr
                (th "Author")
                (td (cvt:var (@ (name "buch.author")))))
              (tr
                (th "Description")
                (td (cvt:var (@ (name "buch.desc"))))))))))))

(define doc-tr1-17-out
'(*TOP*
  (@
    (*NAMESPACES*
      (#f "http://www.w3.org/1999/xhtml")
      (cvt "http://xmlns.therebetygers.net/civet/0.1")))
  (html
    (@ (xml:lang "en") (lang "en"))
    (head (title))
      (body 
        (div
          (@ (class "bookinfo"))
          (table
            (tr
              (th "Title")
              (td "A Study in Scarlet"))
            (tr
              (th "Author")
              (td "Sir Arthur Conan Doyle"))
            (tr
              (th "Description")
              (td "In which we encounter the extraordinary &amp; eccentric Sherlock Holmes for the first time."))))))))

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
    doc-tr1-4-out
    (process-base-template doc-tr1-4-in '() ctx-tr1-4))
  (test
    "TR1.05: cvt:if + else clause, literal string"
    doc-tr1-5-out
    (process-base-template doc-tr1-5-in '() ctx-tr1-4))
  (test
    "TR1.06: cvt:if, no else clause, string variable only"
    doc-tr1-6-out
    (process-base-template doc-tr1-6-in '() ctx-tr1-4))
  (test
    "TR1.07: cvt:if + else clause, string variable only"
    doc-tr1-7-out
    (process-base-template doc-tr1-7-in '() ctx-tr1-4))
  (test
    "TR1.08: cvt:if, no else clause, string var in literal element"
    doc-tr1-8-out
    (process-base-template doc-tr1-8-in '() ctx-tr1-4))
  (test
    "TR1.09: cvt:if + else clause, string var in literal element"
    doc-tr1-9-out
    (process-base-template doc-tr1-9-in '() ctx-tr1-4))
  (test
    "TR1.10: cvt:if, insert two vars, each in its own paragraph"
    doc-tr1-10-out
    (process-base-template doc-tr1-10-in '() ctx-tr1-4))
  (test
    "TR1.11: direct attribute substitution"
    doc-tr1-11-out
    (process-base-template doc-tr1-11-in '() ctx-tr1-4))
  (test
    "TR1.12: attribute substitution using cvt:attr elements"
    doc-tr1-12-out
    (process-base-template doc-tr1-12-in '() ctx-tr1-4))
  (test
    "TR1.13: cvt:for, insert text nodes"
    doc-tr1-13-out
    (process-base-template doc-tr1-13-in '() ctx-tr1-13))
  (test
    "TR1.14: cvt:for, unsorted, insert text in a <p>"
    doc-tr1-14-out
    (process-base-template doc-tr1-14-in '() ctx-tr1-13))
  (test
    "TR1.15: cvt:var with object variable"
    doc-tr1-15-out
    (process-base-template doc-tr1-15-in '() ctx-tr1-13))
  (test
    "TR1.16: cvt:for with sort-field, inserting object fields"
    doc-tr1-16-out
    (process-base-template doc-tr1-16-in '() ctx-tr1-13))
  (test
    "TR1.17: cvt:for over a one-item list, inserting object fields"
    doc-tr1-17-out
    (process-base-template doc-tr1-17-in '() ctx-tr1-13)))

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

(define doc-tr2-2
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

(define doc-tr2-3-base-xml
#<<XML
<html
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:cvt="http://xmlns.therebetygers.net/civet/0.1"
    xml:lang="en" lang="en">
  <head>
    <title></title>
  </head>
  <body>
    <cvt:block name="a">
      <p>Block A from the base template.</p>
    </cvt:block>
    <cvt:block name="b">
      <p>Block B from the base template.</p>
    </cvt:block>
    <cvt:block name="c">
      <p>Block C from the base template.</p>
    </cvt:block>
    <cvt:block name="d">
      Block D from the base template.
    </cvt:block>
    <cvt:block name="e">
      <p>Block E from the base template.</p>
    </cvt:block>
  </body>
</html>
XML
)

(define doc-tr2-3-base-sxml
  '(*TOP*
    (@
      (*NAMESPACES*
        (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
    (html
      (@
        (xml:lang "en")
        (lang "en"))
      (head (title))
      (body
        (cvt:block
          (@ (name "a"))
          (p "Block A from the base template."))
        (cvt:block
          (@ (name "b"))
          (p "Block B from the base template."))
        (cvt:block
          (@ (name "c"))
          (p "Block C from the base template."))
        (cvt:block
          (@ (name "d"))
          "Block D from the base template.")
        (cvt:block
          (@ (name "e"))
          (p "Block E from the base template."))))))

(define doc-tr2-3-ext1-xml
#<<XML
<cvt:template
    extends="doc-tr2-3-base.html"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:cvt="http://xmlns.therebetygers.net/civet/0.1">
  <cvt:block name="a">
    <p>Block A from extension template 1.</p>
  </cvt:block>
  <cvt:block name="c">
    Block C from extension template 1.
  </cvt:block>
</cvt:template>
XML
)

(define doc-tr2-3-ext1-sxml
  '(*TOP*
    (@
      (*NAMESPACES*
        (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
    (cvt:template
      (@
        (extends "tr2-3-base.html"))
      (cvt:block
        (@ (name "a"))
        (p "Block A from extension template 1."))
      (cvt:block
        (@ (name "c"))
        "Block C from extension template 1."))))

(define doc-tr2-3-ext2-xml
#<<XML
<cvt:template
    extends="doc-tr2-3-ext1.html"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:cvt="http://xmlns.therebetygers.net/civet/0.1">
  <cvt:block name="c">
    <p>Block C from extension template 2.</p>
  </cvt:block>
  <cvt:block name="e">
    Block E from extension template 2.
  </cvt:block>
</cvt:template>
XML
)

(define doc-tr2-3-ext2-sxml
  '(*TOP*
    (@
      (*NAMESPACES*
        (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
    (cvt:template
      (@
        (extends "tr2-3-ext1.html"))
      (cvt:block
        (@ (name "c"))
        (p "Block C from extension template 2."))
      (cvt:block
        (@ (name "e"))
        "Block E from extension template 2."))))

(define doc-tr2-3-out
  '(*TOP*
    (@
      (*NAMESPACES*
        (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.1")))
    (html
      (@
        (xml:lang "en")
        (lang "en"))
      (head (title))
      (body
        (p "Block A from extension template 1.")
        (p "Block B from the base template.")
        (p "Block C from extension template 2.")
        "Block D from the base template."
        "Block E from extension template 2."))))

(define ctx-tr2-4
  (make-context
    vars: '((copyrightYear . "2012-2013")
            (copyrightHolder . "Brenda B. Brenner")
            (rightsStatement . "You have no rights."))))

(define doc-tr2-4-ext2-xml
#<<XML
<cvt:template
    extends="doc-tr2-3-ext1.html"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:cvt="http://xmlns.therebetygers.net/civet/0.1">
  <cvt:head>
    <cvt:defvar name="copyrightStatement">
      Copyright © <cvt:var name="copyrightYear" /> by <cvt:var name="copyrightHolder" />. <cvt:var name="rightsStatement" />
    </cvt:defvar>
  </cvt:head>
  <cvt:block name="c">
    <p>Block C from extension template 2.</p>
  </cvt:block>
  <cvt:block name="e">
    Block E from extension template 2.
  </cvt:block>
</cvt:template>
XML
)

(define doc-tr2-4-ext2-sxml
  '(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
                          (cvt "http://xmlns.therebetygers.net/civet/0.1")))
         (cvt:template (@ (extends "doc-tr2-3-ext1.html"))
                       (cvt:head
                         (cvt:defvar (@ (name "copyrightStatement"))
                                     "\n      Copyright © "
                                     (cvt:var (@ (name "copyrightYear")))
                                     " by "
                                     (cvt:var (@ (name "copyrightHolder")))
                                     ".\n      "
                                     (cvt:var (@ (name "rightsStatement")))))
                       (cvt:block (@ (name "c"))
                                  (p "Block C from extension template 2."))
                       (cvt:block (@ (name "e"))
                                  "\n    Block E from extension template 2.\n  "))))


(define ctx-tr2-5
  (make-context
    vars: '((copyrightYear . "2012-2013") (copyrightHolder . "Brenda B. Brenner")
            (rightsStatement . "You have no rights.") (urlScheme . "http")
            (host . "some.blog.com") (articleID . "cafebabe"))))

(define doc-tr2-5-ext2-xml
#<<XML
<cvt:template
    extends="doc-tr2-3-ext1.html"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:cvt="http://xmlns.therebetygers.net/civet/0.1">
  <cvt:head>
    <cvt:locale lang="ja" country="jp" encoding="shift-jis" />
    <cvt:defvar name="copyrightStatement">
      Copyright © <cvt:var name="copyrightYear" /> by <cvt:var name="copyrightHolder" />. <cvt:var name="rightsStatement" />
    </cvt:defvar>
    <cvt:defvar name="junk" value="hullo" />
    <cvt:defvar name="articleURL">
      <cvt:var name="urlScheme" />://<cvt:var name="host" />/articles/<cvt:var name="articleID" />
    </cvt:defvar>
  </cvt:head>
  <cvt:block name="c">
    <p>Block C from extension template 2.</p>
  </cvt:block>
  <cvt:block name="e">
    <a cvt:href="articleURL">Check out this great article!</a>
  </cvt:block>
</cvt:template>
XML
)

(define doc-tr2-5-ext2-sxml
  '(*TOP* (@ (*NAMESPACES*
               (#f "http://www.w3.org/1999/xhtml")
               (cvt "http://xmlns.therebetygers.net/civet/0.1")))
    (cvt:template
      (@ (extends "doc-tr2-3-ext1.html"))
      (cvt:head
        (cvt:locale (@ (lang "ja") (encoding "shift-jis") (country "jp")))
        (cvt:defvar (@ (name "copyrightStatement"))
                    "\n      Copyright © "
                    (cvt:var (@ (name "copyrightYear")))
                    " by "
                    (cvt:var (@ (name "copyrightHolder")))
                    ".\n      "
                    (cvt:var (@ (name "rightsStatement"))))
        (cvt:defvar (@ (value "hullo") (name "junk")))
        (cvt:defvar (@ (name "articleURL"))
                    (cvt:var (@ (name "urlScheme")))
                    "://"
                    (cvt:var (@ (name "host")))
                    "/articles/"
                    (cvt:var (@ (name "articleID")))))
      (cvt:block (@ (name "c"))
                 (p "Block C from extension template 2."))
      (cvt:block (@ (name "e"))
                 (a (@ (cvt:href "articleURL"))
                    "Check out this great article!")))))

;;; ========================================================================
;;; ------  Run tests  -----------------------------------------------------

(test-group "[TR2] Template Sets"
  (test-error
    "TR2.01: cvt:template with no 'extends' attribute [ERROR]"
    (process-base-template doc-tr2-1 '() ctx-tr2-1))
  (test-error
    "TR2.02: cvt:template as descendant of the document element [ERROR]"
    (process-base-template doc-tr2-2 '() ctx-tr2-1))
  (current-test-comparator block-data=?)
  (test
    "TR2.03: build-template-set"
    `(,doc-tr2-3-base-sxml
       ((a . (() () (cvt:block (@ (name "a")) (p "Block A from extension template 1."))))
        (c . (() () (cvt:block (@ (name "c")) (p "Block C from extension template 2."))))
        (e . (() () (cvt:block (@ (name "e")) "Block E from extension template 2.")))))
    (begin
      (with-output-to-file "templates/doc-tr2-3-base.html"
                           (lambda () (display doc-tr2-3-base-xml)))
      (with-output-to-file "templates/doc-tr2-3-ext1.html"
                           (lambda () (display doc-tr2-3-ext1-xml)))
      (with-output-to-file "templates/doc-tr2-3-ext2.html"
                           (lambda () (display doc-tr2-3-ext2-xml)))
      (let-values (((base blox)
                    (build-template-set "doc-tr2-3-ext2.html" (make-context))))
        (list base blox))))
  (test
    "TR2.04: build-template-set with one defvar"
    `(,doc-tr2-3-base-sxml
       ((a . (() ()
              (cvt:block (@ (name "a")) (p "Block A from extension template 1."))))
        (c . (()
              ((copyrightStatement . "Copyright © 2012-2013 by Brenda B. Brenner. You have no rights."))
              (cvt:block (@ (name "c")) (p "Block C from extension template 2."))))
        (e . (()
              ((copyrightStatement . "Copyright © 2012-2013 by Brenda B. Brenner. You have no rights."))
              (cvt:block (@ (name "e")) "Block E from extension template 2.")))))
    (begin
      ; (with-output-to-file "templates/doc-tr2-3-base.html"
                           ; (lambda () (display doc-tr2-3-base-xml)))
      ; (with-output-to-file "templates/doc-tr2-3-ext1.html"
                           ; (lambda () (display doc-tr2-3-ext1-xml)))
      (with-output-to-file "templates/doc-tr2-4-ext2.html"
                           (lambda () (display doc-tr2-4-ext2-xml)))
      (let-values (((base blox)
                    (build-template-set "doc-tr2-4-ext2.html" ctx-tr2-4)))
        (list base blox))))
  (test
    "TR2.05: build-template-set with a locale def and 3 defvars"
    `(,doc-tr2-3-base-sxml
       ((a . (() () (cvt:block (@ (name "a")) (p "Block A from extension template 1."))))
        (c . (((lang . "ja") (encoding . "shift-jis") (country . "jp"))
              ((copyrightStatement . "Copyright © 2012-2013 by Brenda B. Brenner. You have no rights.")
               (junk . "hullo")
               (articleURL . "http://some.blog.com/articles/cafebabe"))
              (cvt:block (@ (name "c")) (p "Block C from extension template 2."))))
        (e . (((lang . "ja") (encoding . "shift-jis") (country . "jp"))
              ((copyrightStatement . "Copyright © 2012-2013 by Brenda B. Brenner. You have no rights.")
               (junk . "hullo")
               (articleURL . "http://some.blog.com/articles/cafebabe"))
              (cvt:block (@ (name "e")) (a (@ (cvt:href "articleURL")) "Check out this great article!"))))))
    (begin
      ; (with-output-to-file "templates/doc-tr2-3-base.html"
                           ; (lambda () (display doc-tr2-3-base-xml)))
      ; (with-output-to-file "templates/doc-tr2-3-ext1.html"
                           ; (lambda () (display doc-tr2-3-ext1-xml)))
      (with-output-to-file "templates/doc-tr2-5-ext2.html"
                           (lambda () (display doc-tr2-5-ext2-xml)))
      (let-values (((base blox)
                    (build-template-set "doc-tr2-5-ext2.html" ctx-tr2-5)))
        (list base blox))))
  (current-test-comparator equal?))


;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  COMPLEX TEMPLATE PROCESSING  -------------------------------------


;;; ------------------------------------------------------------------------

;;; ========================================================================
;;; ------  Run tests  -----------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  RENDERING  -------------------------------------------------------
;;; ------  Support data  --------------------------------------------------

;;; ========================================================================
;;; ------  Run tests  -----------------------------------------------------

(test-group "Rendering"
  (test-assert
    "Minimal example: Single template."
    (render "doc-tr2-3-base.html" (make-context) file: "doc-tr2-3-base-out.html"))
  (test-assert
    "Template set with blocks, no variables."
    (render "doc-tr2-3-ext2.html" (make-context) file: "doc-tr2-3-ext2-out.html"))
  (test-assert
    "Template set w/ base, 2 extensions, variables, & locale"
    (render "doc-tr2-5-ext2.html" ctx-tr2-5 file: "doc-tr2-5-ext2-out.html")))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


(test-exit)

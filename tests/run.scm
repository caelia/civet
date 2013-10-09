(use civet)
(use test)
(use posix files)
(include "test-support.scm")

(create-directory (make-pathname "templates" ".cache") #t)

(define (standard-test-context vars)
  (make-context state: 'init
                nsmap: '((#f . "http://www.w3.org/1999/xhtml")
                         (cvt . "http://xmlns.therebetygers.net/civet/0.2"))
                vars:  vars))

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
  (test "Retrieve a variable" #\L (ctx01 'get-var "louise"))
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

(define ctx-tr10-1
  (standard-test-context '((color . "green"))))

(define doc-tr10-1
  '(*TOP*
     (@
       (*NAMESPACES* 
         (#f "http://www.w3.org/1999/xhtml")
         (cvt "http://xmlns.therebetygers.net/civet/0.2")))
     (html
       (head
         (title "test doc"))
       (body
         (p "Pointless text")))))

(define doc-tr10-2-in
  '(*TOP*
     (@
       (*NAMESPACES* 
         (#f "http://www.w3.org/1999/xhtml")
         (cvt "http://xmlns.therebetygers.net/civet/0.2")))
     (html
       (head
         (title "test doc"))
       (body
         (p
           (cvt:var (@ (name "color"))))))))

(define doc-tr10-2-out
  '(*TOP*
     (@
       (*NAMESPACES* 
         (#f "http://www.w3.org/1999/xhtml")
         (cvt "http://xmlns.therebetygers.net/civet/0.2")))
     (html
       (head
         (title "test doc"))
       (body
         (p "green")))))

(define ctx-tr10-4
  (standard-test-context
    '((color . "lime") (size . "12") (age . "27") (divclass . "Poinsettia")
      (divid . "baz451") (chapeau . "porkpie") (my_hat . "fedora"))))

(define doc-tr10-4-in
'(*TOP*
   (@ (*NAMESPACES*
        (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (cvt:if
             (@ (test "color"))
             "12")))))

(define doc-tr10-4-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body "12"))))

(define doc-tr10-5-in
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (cvt:if
             (@ (test "bolour"))
             "12"
             (cvt:else "27"))))))

(define doc-tr10-5-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body "27"))))

(define doc-tr10-6-in
'(*TOP*
   (@ (*NAMESPACES*
        (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (cvt:if
             (@ (test "color"))
             (cvt:var (@ (name "size"))))))))

(define doc-tr10-6-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body "12"))))

(define doc-tr10-7-in
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (cvt:if
             (@ (test "bolour"))
             (cvt:var (@ (name "size")))
             (cvt:else
               (cvt:var (@ (name "age")))))))))

(define doc-tr10-7-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body "27"))))

(define doc-tr10-8-in
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (cvt:if
             (@ (test "color"))
             (p (cvt:var (@ (name "size")))))))))

(define doc-tr10-8-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (p "12")))))

(define doc-tr10-9-in
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (cvt:if
             (@ (test "bolour"))
             (p (cvt:var (@ (name "size"))))
             (cvt:else
               (div (@ (id "age-div") (class "info"))
                    (cvt:var (@ (name "age"))))))))))

(define doc-tr10-9-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (div (@ (id "age-div") (class "info"))
                "27")))))

(define doc-tr10-10-in
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (cvt:if
             (@ (test "color"))
             (p (cvt:var (@ (name "size"))))
             (p (cvt:var (@ (name "age")))))))))

(define doc-tr10-10-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (p "12")
           (p "27")))))

(define doc-tr10-11-in
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body (div (@ (cvt:class "divclass")) "Div Contents")))))

(define doc-tr10-11-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body (div (@ (class "Poinsettia")) "Div Contents")))))

(define doc-tr10-12-in
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (div
             (@ (id "fubar79") (class "Poinsettia"))
             (cvt:attr (@ (name "id"))
                       (cvt:var (@ (name "divid"))))
             (cvt:attr (@ (var "my_hat") (name "chapeau")))
             "Div Contents")))))

(define doc-tr10-12-out
'(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body
           (div
             (@ (class "Poinsettia") (id "baz451") (chapeau "fedora"))
             "Div Contents")))))

(define ctx-tr10-13
  (standard-test-context
    '((lizards . ("gecko" "komodo dragon" "gila monster" "chameleon"))
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


(define doc-tr10-13-in
'(*TOP*
  (@
    (*NAMESPACES*
      (#f "http://www.w3.org/1999/xhtml")
      (cvt "http://xmlns.therebetygers.net/civet/0.2")))
  (html
    (@ (xml:lang "en") (lang "en"))
   
    (head (title))
      (body 
        (cvt:for
          (@ (in "lizards") (each "x"))
          (cvt:var
            (@ (name "x")))
          (br))))))

(define doc-tr10-13-out
'(*TOP*
  (@
    (*NAMESPACES*
      (#f "http://www.w3.org/1999/xhtml")
      (cvt "http://xmlns.therebetygers.net/civet/0.2")))
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

(define doc-tr10-14-in
'(*TOP*
  (@
    (*NAMESPACES*
      (#f "http://www.w3.org/1999/xhtml")
      (cvt "http://xmlns.therebetygers.net/civet/0.2")))
  (html
    (@ (xml:lang "en") (lang "en"))
    (head (title))
      (body 
        (cvt:for
          (@ (in "books") (each "book") (sort "none"))
          (p
            (cvt:var
              (@ (name "book")))))))))

(define doc-tr10-14-out
'(*TOP*
  (@
    (*NAMESPACES*
      (#f "http://www.w3.org/1999/xhtml")
      (cvt "http://xmlns.therebetygers.net/civet/0.2")))
  (html
    (@ (xml:lang "en") (lang "en"))
    (head (title))
      (body 
        (p "Frankenstein")
        (p "The Wise Man's Fear")
        (p "The Left Hand of Darkness")
        (p "One Hundred Years of Solitude")
        (p "Disaster Capitalism")))))

(define doc-tr10-15-in
'(*TOP*
  (@
    (*NAMESPACES*
      (#f "http://www.w3.org/1999/xhtml")
      (cvt "http://xmlns.therebetygers.net/civet/0.2")))
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

(define doc-tr10-15-out
'(*TOP*
  (@
    (*NAMESPACES*
      (#f "http://www.w3.org/1999/xhtml")
      (cvt "http://xmlns.therebetygers.net/civet/0.2")))
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

(define doc-tr10-16-in
'(*TOP*
  (@
    (*NAMESPACES*
      (#f "http://www.w3.org/1999/xhtml")
      (cvt "http://xmlns.therebetygers.net/civet/0.2")))
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

(define doc-tr10-16-out
'(*TOP*
  (@
    (*NAMESPACES*
      (#f "http://www.w3.org/1999/xhtml")
      (cvt "http://xmlns.therebetygers.net/civet/0.2")))
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

(define doc-tr10-17-in
'(*TOP*
  (@
    (*NAMESPACES*
      (#f "http://www.w3.org/1999/xhtml")
      (cvt "http://xmlns.therebetygers.net/civet/0.2")))
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

(define doc-tr10-17-out
'(*TOP*
  (@
    (*NAMESPACES*
      (#f "http://www.w3.org/1999/xhtml")
      (cvt "http://xmlns.therebetygers.net/civet/0.2")))
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

(define ctx-tr10-18
  (standard-test-context '((base_url . "http://some.cool.com/pages"))))

(define doc-tr10-18-in
'(*TOP*
   (@
     (*NAMESPACES*
       (#f "http://www.w3.org/1999/xhtml")
       (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html
     (@ (xml:lang "en") (lang "en"))
     (head (title))
     (body
       (cvt:with
         (cvt:defvar
           (@ (value "articles") (name "local_part")))
         (p
           (cvt:var
             (@ (name "base_url")))
           "/"
           (cvt:var
             (@ (name "local_part")))))
       (cvt:with
         (cvt:defvar
           (@ (value "people") (name "local_part")))
         (p
           (cvt:var
             (@ (name "base_url")))
           "/"
           (cvt:var
             (@ (name "local_part")))))
       (cvt:with
         (cvt:defvar
           (@ (value "admin") (name "local_part")))
         (p
           (cvt:var
             (@ (name "base_url")))
           "/"
           (cvt:var
             (@ (name "local_part")))))))))

(define doc-tr10-18-out
'(*TOP* 
   (@ 
     (*NAMESPACES*
       (#f "http://www.w3.org/1999/xhtml")
       (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html 
     (@ (xml:lang "en") (lang "en"))
     (head (title))
     (body 
       (p "http://some.cool.com/pages" "/" "articles")
       (p "http://some.cool.com/pages" "/" "people")
       (p "http://some.cool.com/pages" "/" "admin")))))

(define doc-tr10-19-in
'(*TOP*
   (@
     (*NAMESPACES*
       (#f "http://www.w3.org/1999/xhtml")
       (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html
     (@ (xml:lang "en") (lang "en"))
     (head (title))
     (body
       (cvt:with
         (cvt:defvar
           (@ (value "articles") (name "local_part")))
         (p
           (cvt:var
             (@ (name "base_url")))
           "/"
           (cvt:var
             (@ (name "local_part")))))
       (p 
         (cvt:var 
           (@ (name "base_url")))
         "/"
         (cvt:var 
           (@ (name "local_part"))))))))

(define ctx-tr10-20
  (standard-test-context '((names . ("Sally")))))

(define doc-tr10-20-in
'(*TOP* 
   (@ 
     (*NAMESPACES*
       (#f "http://www.w3.org/1999/xhtml")
       (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html 
     (@ (xml:lang "en") (lang "en"))
     (head (title))
     (body 
       (cvt:for
         (@ (in "names") (each "name"))
         (cvt:interpolate ", ")
         (cvt:interpolate (@ (mode "pair")) " & ")
         (cvt:interpolate (@ (mode "last")) ", & ")
         (cvt:var (@ (name "name"))))))))

(define doc-tr10-20-out
'(*TOP* 
   (@ 
     (*NAMESPACES*
       (#f "http://www.w3.org/1999/xhtml")
       (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html 
     (@ (xml:lang "en") (lang "en"))
     (head (title))
     (body "Sally"))))

(define ctx-tr10-21
  (standard-test-context '((names . ("Sally" "Dorian")))))

(define doc-tr10-21-out
'(*TOP* 
   (@ 
     (*NAMESPACES*
       (#f "http://www.w3.org/1999/xhtml")
       (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html 
     (@ (xml:lang "en") (lang "en"))
     (head (title))
     (body "Dorian" " & " "Sally"))))

(define ctx-tr10-22
  (standard-test-context '((names . ("Amanda" "Brendan" "Caroline" "Daniel")))))

(define doc-tr10-22-out
'(*TOP* 
   (@ 
     (*NAMESPACES*
       (#f "http://www.w3.org/1999/xhtml")
       (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html 
     (@ (xml:lang "en") (lang "en"))
     (head (title))
     (body "Amanda" ", " "Brendan" ", " "Caroline" ", & " "Daniel"))))

(define doc-tr10-23-in
'(*TOP* 
   (@ 
     (*NAMESPACES*
       (#f "http://www.w3.org/1999/xhtml")
       (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html 
     (@ (xml:lang "en") (lang "en"))
     (cvt:head
       (cvt:defmacro
         (@ (name "local_url"))
         (p 
           (cvt:var 
             (@ (name "base_url")))
           "/"
           (cvt:var 
             (@ (name "local_part"))))))
     (head (title))
     (body 
       (cvt:with
         (cvt:defvar 
           (@ (value "articles") (name "local_part")))
         (cvt:macro 
           (@ (name "local_url"))))
       (cvt:with
         (cvt:defvar 
           (@ (value "people") (name "local_part")))
         (cvt:macro 
           (@ (name "local_url"))))
       (cvt:with
         (cvt:defvar 
           (@ (value "admin") (name "local_part")))
         (cvt:macro 
           (@ (name "local_url"))))))))

;;; ========================================================================
;;; ------  Run tests  -----------------------------------------------------

(test-group "[TR10] Simple Transformations"
  (test
    "TR10.01: Simple Template without CVT Markup (identity)"
    min-doc
    (process-base-template min-doc '() min-ctx))
  (test
    "TR10.02: XHTML Template without CVT Markup (identity)"
    doc-tr10-1
    (process-base-template doc-tr10-1 '() ctx-tr10-1))
  (test
    "TR10.03: Basic string variable substitution"
    doc-tr10-2-out
    (process-base-template doc-tr10-2-in '() ctx-tr10-1))
  (test
    "TR10.04: cvt:if, no else clause, literal string"
    doc-tr10-4-out
    (process-base-template doc-tr10-4-in '() ctx-tr10-4))
  (test
    "TR10.05: cvt:if + else clause, literal string"
    doc-tr10-5-out
    (process-base-template doc-tr10-5-in '() ctx-tr10-4))
  (test
    "TR10.06: cvt:if, no else clause, string variable only"
    doc-tr10-6-out
    (process-base-template doc-tr10-6-in '() ctx-tr10-4))
  (test
    "TR10.07: cvt:if + else clause, string variable only"
    doc-tr10-7-out
    (process-base-template doc-tr10-7-in '() ctx-tr10-4))
  (test
    "TR10.08: cvt:if, no else clause, string var in literal element"
    doc-tr10-8-out
    (process-base-template doc-tr10-8-in '() ctx-tr10-4))
  (test
    "TR10.09: cvt:if + else clause, string var in literal element"
    doc-tr10-9-out
    (process-base-template doc-tr10-9-in '() ctx-tr10-4))
  (test
    "TR10.10: cvt:if, insert two vars, each in its own paragraph"
    doc-tr10-10-out
    (process-base-template doc-tr10-10-in '() ctx-tr10-4))
  (test
    "TR10.11: direct attribute substitution"
    doc-tr10-11-out
    (process-base-template doc-tr10-11-in '() ctx-tr10-4))
  (test
    "TR10.12: attribute substitution using cvt:attr elements"
    doc-tr10-12-out
    (process-base-template doc-tr10-12-in '() ctx-tr10-4))
  (test
    "TR10.13: cvt:for, insert text nodes"
    doc-tr10-13-out
    (process-base-template doc-tr10-13-in '() ctx-tr10-13))
  (test
    "TR10.14: cvt:for, unsorted, insert text in a <p>"
    doc-tr10-14-out
    (process-base-template doc-tr10-14-in '() ctx-tr10-13))
  (test
    "TR10.15: cvt:var with object variable"
    doc-tr10-15-out
    (process-base-template doc-tr10-15-in '() ctx-tr10-13))
  (test
    "TR10.16: cvt:for with sort-field, inserting object fields"
    doc-tr10-16-out
    (process-base-template doc-tr10-16-in '() ctx-tr10-13))
  (test
    "TR10.17: cvt:for over a one-item list, inserting object fields"
    doc-tr10-17-out
    (process-base-template doc-tr10-17-in '() ctx-tr10-13))
  (test
    "TR10.18: insert variables defined in cvt:with"
    doc-tr10-18-out
    (process-base-template doc-tr10-18-in '() ctx-tr10-18))
  (test-error
    "TR10.19: referencing variables out of cvt:with scope [ERROR]"
    (process-base-template doc-tr10-19-in '() ctx-tr10-18))
  (test
    "TR10.20: cvt:interpolate with a loop variable of length 1."
    doc-tr10-20-out
    (process-base-template doc-tr10-20-in '() ctx-tr10-20))
  (test
    "TR10.21: cvt:interpolate with a loop variable of length 2."
    doc-tr10-21-out
    (process-base-template doc-tr10-20-in '() ctx-tr10-21))
  (test
    "TR10.22: cvt:interpolate with a loop variable of length > 2."
    doc-tr10-22-out
    (process-base-template doc-tr10-20-in '() ctx-tr10-22))
  (test
    "TR10.23: use cvt:macro to insert a local variable"
    doc-tr10-18-out
    (process-base-template
      doc-tr10-23-in
      '((fake .
        (() ()
         ((local_url . (p (cvt:var (@ (name "base_url"))) "/" (cvt:var (@ (name "local_part"))))))
         ())))
      ctx-tr10-18)))


;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  STRING FORMATTING  -----------------------------------------------
;;; ------  Support data  --------------------------------------------------

(define ctx-tr11-1
  (standard-test-context '((tag . "water balloons"))))

(define doc-tr11-1-in
'(*TOP* 
   (@ 
     (*NAMESPACES*
       (#f "http://www.w3.org/1999/xhtml")
       (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body 
           (p 
             "Tag: "
             (cvt:var 
               (@ (name "tag") (format "uri"))))))))

(define doc-tr11-1-out
'(*TOP* 
   (@ 
     (*NAMESPACES*
       (#f "http://www.w3.org/1999/xhtml")
       (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html 
     (@ (xml:lang "en") (lang "en"))
     (head (title))
     (body (p "Tag: " "water%20balloons")))))

(define ctx-tr11-2
  (standard-test-context '((tag . "water_balloons"))))

(define doc-tr11-2-out
'(*TOP* 
   (@ 
     (*NAMESPACES*
       (#f "http://www.w3.org/1999/xhtml")
       (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html 
     (@ (xml:lang "en") (lang "en"))
     (head (title))
     (body (p "Tag: " "water_balloons")))))

(define ctx-tr11-3
  (standard-test-context '((weird_uri . "http://some.uri.com/with white space"))))

(define doc-tr11-3-in
'(*TOP* 
   (@ 
     (*NAMESPACES*
       (#f "http://www.w3.org/1999/xhtml")
       (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html (@ (xml:lang "en") (lang "en"))
         (head (title))
         (body 
           (a
             (cvt:attr
               (@ (name "href") (var "weird_uri") (type "uri")))
             "Link Text")))))

(define doc-tr11-3-out
'(*TOP* 
   (@ 
     (*NAMESPACES*
       (#f "http://www.w3.org/1999/xhtml")
       (cvt "http://xmlns.therebetygers.net/civet/0.2")))
   (html 
     (@ (xml:lang "en") (lang "en"))
     (head (title))
     (body 
       (a
         (@ (href "http://some.uri.com/with%20white%20space"))
         "Link Text")))))

;;; ========================================================================
;;; ------  Run tests  -----------------------------------------------------

(test-group "[TR11] Formatted Variable Insertion"
  (test
    "TR11.01: uri-encode a string with spaces"
    doc-tr11-1-out
    (process-base-template doc-tr11-1-in '() ctx-tr11-1))
  (test
    "TR11.02: uri-encode a string without spaces"
    doc-tr11-2-out
    (process-base-template doc-tr11-1-in '() ctx-tr11-2))
  (test
    "TR11.03: uri-encode an inserted attribute."
    doc-tr11-3-out
    (process-base-template doc-tr11-3-in '() ctx-tr11-3)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  TEMPLATE SET CONSTRUCTION  ---------------------------------------
;;; ------  Support data  --------------------------------------------------

(define ctx-tr20-1
  (make-context state: 'init
                nsmap: '((#f . "http://www.w3.org/1999/xhtml")
                         (cvt . "http://xmlns.therebetygers.net/civet/0.2"))))

(define doc-tr20-1
  '(*TOP*
     (@
       (*NAMESPACES* 
         (#f "http://www.w3.org/1999/xhtml")
         (cvt "http://xmlns.therebetygers.net/civet/0.2")))
     (cvt:template
       (html
         (head
           (title "Bad template"))
         (body
           (p "Should never see the light of day"))))))

(define doc-tr20-2
  '(*TOP*
     (@
       (*NAMESPACES* 
         (#f "http://www.w3.org/1999/xhtml")
         (cvt "http://xmlns.therebetygers.net/civet/0.2")))
     (html
       (cvt:template
         (@
           (extends "fubar.html"))
         (head
           (title "Bad template"))
         (body
           (p "Should never see the light of day"))))))

(define doc-tr20-3-base-xml
#<<XML
<html
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:cvt="http://xmlns.therebetygers.net/civet/0.2"
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

(define doc-tr20-3-base-sxml
  '(*TOP*
    (@
      (*NAMESPACES*
        (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.2")))
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

(define doc-tr20-3-ext1-xml
#<<XML
<cvt:template
    extends="doc-tr20-3-base.html"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:cvt="http://xmlns.therebetygers.net/civet/0.2">
  <cvt:block name="a">
    <p>Block A from extension template 1.</p>
  </cvt:block>
  <cvt:block name="c">
    Block C from extension template 1.
  </cvt:block>
</cvt:template>
XML
)

(define doc-tr20-3-ext1-sxml
  '(*TOP*
    (@
      (*NAMESPACES*
        (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.2")))
    (cvt:template
      (@
        (extends "tr20-3-base.html"))
      (cvt:block
        (@ (name "a"))
        (p "Block A from extension template 1."))
      (cvt:block
        (@ (name "c"))
        "Block C from extension template 1."))))

(define doc-tr20-3-ext2-xml
#<<XML
<cvt:template
    extends="doc-tr20-3-ext1.html"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:cvt="http://xmlns.therebetygers.net/civet/0.2">
  <cvt:block name="c">
    <p>Block C from extension template 2.</p>
  </cvt:block>
  <cvt:block name="e">
    Block E from extension template 2.
  </cvt:block>
</cvt:template>
XML
)

(define doc-tr20-3-ext2-sxml
  '(*TOP*
    (@
      (*NAMESPACES*
        (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.2")))
    (cvt:template
      (@
        (extends "tr20-3-ext1.html"))
      (cvt:block
        (@ (name "c"))
        (p "Block C from extension template 2."))
      (cvt:block
        (@ (name "e"))
        "Block E from extension template 2."))))

(define doc-tr20-3-out
  '(*TOP*
    (@
      (*NAMESPACES*
        (#f "http://www.w3.org/1999/xhtml")
        (cvt "http://xmlns.therebetygers.net/civet/0.2")))
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

(define ctx-tr20-4
  (make-context
    vars: '((copyrightYear . "2012-2013")
            (copyrightHolder . "Brenda B. Brenner")
            (rightsStatement . "You have no rights."))))

(define doc-tr20-4-ext2-xml
#<<XML
<cvt:template
    extends="doc-tr20-3-ext1.html"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:cvt="http://xmlns.therebetygers.net/civet/0.2">
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

(define doc-tr20-4-ext2-sxml
  '(*TOP* (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
                          (cvt "http://xmlns.therebetygers.net/civet/0.2")))
         (cvt:template (@ (extends "doc-tr20-3-ext1.html"))
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


(define ctx-tr20-5
  (make-context
    vars: '((copyrightYear . "2012-2013") (copyrightHolder . "Brenda B. Brenner")
            (rightsStatement . "You have no rights.") (urlScheme . "http")
            (host . "some.blog.com") (articleID . "cafebabe"))))

(define doc-tr20-5-ext2-xml
#<<XML
<cvt:template
    extends="doc-tr20-3-ext1.html"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:cvt="http://xmlns.therebetygers.net/civet/0.2">
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

(define doc-tr20-5-ext2-sxml
  '(*TOP* (@ (*NAMESPACES*
               (#f "http://www.w3.org/1999/xhtml")
               (cvt "http://xmlns.therebetygers.net/civet/0.2")))
    (cvt:template
      (@ (extends "doc-tr20-3-ext1.html"))
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

(test-group "[TR20] Template Sets"
  (test-error
    "TR20.01: cvt:template with no 'extends' attribute [ERROR]"
    (process-base-template doc-tr20-1 '() ctx-tr20-1))
  (test-error
    "TR20.02: cvt:template as descendant of the document element [ERROR]"
    (process-base-template doc-tr20-2 '() ctx-tr20-1))
  (current-test-comparator block-data=?)
  (test
    "TR20.03: build-template-set"
    `(,doc-tr20-3-base-sxml
       ((a . (() () () (cvt:block (@ (name "a")) (p "Block A from extension template 1."))))
        (c . (() () () (cvt:block (@ (name "c")) (p "Block C from extension template 2."))))
        (e . (() () () (cvt:block (@ (name "e")) "Block E from extension template 2.")))))
    (begin
      (with-output-to-file "templates/doc-tr20-3-base.html"
                           (lambda () (display doc-tr20-3-base-xml)))
      (with-output-to-file "templates/doc-tr20-3-ext1.html"
                           (lambda () (display doc-tr20-3-ext1-xml)))
      (with-output-to-file "templates/doc-tr20-3-ext2.html"
                           (lambda () (display doc-tr20-3-ext2-xml)))
      (let-values (((base blox)
                    (build-template-set "doc-tr20-3-ext2.html" (make-context))))
        (list base blox))))
  (test
    "TR20.04: build-template-set with one defvar"
    `(,doc-tr20-3-base-sxml
       ((a . (() () ()
              (cvt:block (@ (name "a")) (p "Block A from extension template 1."))))
        (c . (()
              ((copyrightStatement . "Copyright © 2012-2013 by Brenda B. Brenner. You have no rights."))
              ()
              (cvt:block (@ (name "c")) (p "Block C from extension template 2."))))
        (e . (()
              ((copyrightStatement . "Copyright © 2012-2013 by Brenda B. Brenner. You have no rights."))
              ()
              (cvt:block (@ (name "e")) "Block E from extension template 2.")))))
    (begin
      ; (with-output-to-file "templates/doc-tr20-3-base.html"
                           ; (lambda () (display doc-tr20-3-base-xml)))
      ; (with-output-to-file "templates/doc-tr20-3-ext1.html"
                           ; (lambda () (display doc-tr20-3-ext1-xml)))
      (with-output-to-file "templates/doc-tr20-4-ext2.html"
                           (lambda () (display doc-tr20-4-ext2-xml)))
      (let-values (((base blox)
                    (build-template-set "doc-tr20-4-ext2.html" ctx-tr20-4)))
        (list base blox))))
  (test
    "TR20.05: build-template-set with a locale def and 3 defvars"
    `(,doc-tr20-3-base-sxml
       ((a . (() () () (cvt:block (@ (name "a")) (p "Block A from extension template 1."))))
        (c . (((lang . "ja") (encoding . "shift-jis") (country . "jp"))
              ((copyrightStatement . "Copyright © 2012-2013 by Brenda B. Brenner. You have no rights.")
               (junk . "hullo")
               (articleURL . "http://some.blog.com/articles/cafebabe"))
              ()
              (cvt:block (@ (name "c")) (p "Block C from extension template 2."))))
        (e . (((lang . "ja") (encoding . "shift-jis") (country . "jp"))
              ((copyrightStatement . "Copyright © 2012-2013 by Brenda B. Brenner. You have no rights.")
               (junk . "hullo")
               (articleURL . "http://some.blog.com/articles/cafebabe"))
              ()
              (cvt:block (@ (name "e")) (a (@ (cvt:href "articleURL")) "Check out this great article!"))))))
    (begin
      ; (with-output-to-file "templates/doc-tr20-3-base.html"
                           ; (lambda () (display doc-tr20-3-base-xml)))
      ; (with-output-to-file "templates/doc-tr20-3-ext1.html"
                           ; (lambda () (display doc-tr20-3-ext1-xml)))
      (with-output-to-file "templates/doc-tr20-5-ext2.html"
                           (lambda () (display doc-tr20-5-ext2-xml)))
      (let-values (((base blox)
                    (build-template-set "doc-tr20-5-ext2.html" ctx-tr20-5)))
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
    (render "doc-tr20-3-base.html" (make-context) file: "doc-tr20-3-base-out.html"))
  (test-assert
    "Template set with blocks, no variables."
    (render "doc-tr20-3-ext2.html" (make-context) file: "doc-tr20-3-ext2-out.html"))
  (test-assert
    "Template set w/ base, 2 extensions, variables, & locale"
    (render "doc-tr20-5-ext2.html" ctx-tr20-5 file: "doc-tr20-5-ext2-out.html")))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


(test-exit)

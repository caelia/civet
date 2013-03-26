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

;;; ------------------------------------------------------------------------

;;; ========================================================================
;;; ------  Run tests  -----------------------------------------------------



;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  TEMPLATE SET CONSTRUCTION  ---------------------------------------


;;; ------------------------------------------------------------------------

;;; ========================================================================
;;; ------  Run tests  -----------------------------------------------------


;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  COMPLEX TEMPLATE PROCESSING  -------------------------------------


;;; ------------------------------------------------------------------------

;;; ========================================================================
;;; ------  Run tests  -----------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

(test-exit)

(use test)
(include "test-support.scm")

(test-group "sxml=? support functions"
  (test
    "text-node=? with different strings [#f]"
    #f
    (text-node=? "abc" "xyz" #t))
  (test
    "text-node=? with identical strings [#t]"
    #t
    (text-node=? "abc" "abc" #t))
  (test
    "text-node=? with whitespace differences not ignored [#f]"
    #f
    (text-node=? "abc" "     abc      " #f))
  (test
    "text-node=? with whitespace differences ignored [#t]"
    #t
    (text-node=? "abc" "     abc      " #t))
  (test
    "symbol<? where one argument is not a symbol [#f]"
    #f
    (symbol<? 'flange "garbage"))
  (test
    "symbol<? with identical symbols [#f]"
    #f
    (symbol<? 'xylene 'xylene))
  (test
    "symbol<? [#f]"
    #f
    (symbol<? 'yeoman 'xylene))
  (test
    "symbol<? [#t]"
    #t
    (symbol<? 'yeoman 'yeoville))
  (test
    "head-sym<? where one argument is null [#f]"
    #f
    (head-sym<? '() '(a . "15-0-0")))
  (test
    "head-sym<? where one argument is a string [#f]"
    #f
    (head-sym<? '(vitriolic "rhetoric") "and ginger snaps"))
  (test
    "head-sym<? with equal head symbols [#f]" 
    #f
    (head-sym<? '(moronic 49) '(moronic 253)))
  (test
    "(head-sym<? '(euphoria 49) '(fenestration)) [#t]"
    #t
    (head-sym<? '(euphoria 49) '(fenestration)))
  (test
    "(head-sym<? '(euphoria 49) '(fenestration 100)) [#t]"
    #t
    (head-sym<? '(euphoria 49) '(fenestration 100)))
  (test
    "(head-sym<? '(gorgonzola \"cheese\") '(aperture \"control\")) [#f]"
    #f
    (head-sym<? '(gorgonzola "cheese") '(aperture "control")))
  (test
    "(head-sym<? '(euphoria 49) '(fenestration)) [#t]"
    #t
    (head-sym<? '(euphoria 49) '(fenestration)))
  (test
    "att-list=? with identical lists [#t]"
    #t
    (att-list=? '((class "high") (id "lemon") (flower "marigold"))
                '((class "high") (id "lemon") (flower "marigold"))))
  (test
    "att-list=? with identical attributes in different orders [#t]"
    #t
    (att-list=? '((class "high") (id "lemon") (flower "marigold"))
                '((id "lemon") (flower "marigold") (class "high"))))
  (test
    "att-list=? with the same attribute names but one different value [#f]"
    #f
    (att-list=? '((class "high") (id "lemon") (flower "marigold"))
                '((class "high") (id "banana") (flower "marigold"))))
  (test
    "att-list=? with different lists [#f]"
    #f
    (att-list=? '((class "high") (id "lemon") (flower "marigold"))
                '((class "high") (id "lemon")))))

(test-group "sxml=?"
  (test
    "sxml=? with two identical fragments [#t]"
    #t
    (sxml=?
      '(doc
         (@ (class "fubar") (id "f4389"))
         (head
           (title "silly document")
           (meta
             (author "Mark Twain")))
         (body "None whatsoever"))
      '(doc
         (@ (class "fubar") (id "f4389"))
         (head
           (title "silly document")
           (meta
             (author "Mark Twain")))
         (body "None whatsoever"))))
  (test
    "sxml=? with two identical fragments, except for attribute order [#t]"
    #t
    (sxml=?
      '(doc
         (@ (id "f4389") (class "fubar"))
         (head
           (title "silly document")
           (meta
             (author "Mark Twain")))
         (body "None whatsoever"))
      '(doc
         (@ (class "fubar") (id "f4389"))
         (head
           (title "silly document")
           (meta
             (author "Mark Twain")))
         (body "None whatsoever"))))
  (test
    "sxml=? with two identical fragments, except for whitespace [#t]"
    #t
    (sxml=?
      '(doc
         (@ (class "fubar") (id "f4389"))
         (head
           (title "silly document")
           (meta
             (author "   Mark Twain")))
         (body "None whatsoever  "))
      '(doc
         (@ (class "fubar") (id "f4389"))
         (head
           (title "   silly document  ")
           (meta
             (author "Mark Twain")))
         (body "None whatsoever"))))
  (test
    "sxml=? with whitespace differences not ignored [#f]"
    #f
    (sxml=?
      '(doc
         (@ (class "fubar") (id "f4389"))
         (head
           (title "silly document")
           (meta
             (author "   Mark Twain")))
         (body "None whatsoever  "))
      '(doc
         (@ (class "fubar") (id "f4389"))
         (head
           (title "   silly document  ")
           (meta
             (author "Mark Twain")))
         (body "None whatsoever"))
      #f))
  (test
    "sxml=? with differing element order [#f]"
    #f
    (sxml=?
      '(doc
         (@ (class "fubar") (id "f4389"))
         (head
           (meta
             (author "Mark Twain"))
           (title "silly document"))
         (body "None whatsoever"))
      '(doc
         (@ (class "fubar") (id "f4389"))
         (head
           (title "silly document")
           (meta
             (author "Mark Twain")))
         (body "None whatsoever"))))
  (test
    "sxml=? with completely different documents [#f]"
    #f
    (sxml=?
      '(doc
         (@ (class "fubar") (id "f4389"))
         (head
           (meta
             (author "Mark Twain"))
           (title "silly document"))
         (body "None whatsoever"))
      '(goose-egg))))

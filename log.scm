'((*TOP*
    (@ (*NAMESPACES*
         (#f "http://www.w3.org/1999/xhtml")
         (cvt "http://xmlns.therebetygers.net/civet/0.1")))
    (html
      (@ (xml:lang "en") (lang "en")) (head (title))
      (body
        (cvt:block (@ (name "a")) (p "Block A from the base template."))
        (cvt:block (@ (name "b")) (p "Block B from the base template."))
        (cvt:block (@ (name "c")) (p "Block C from the base template."))
        (cvt:block (@ (name "d")) "Block D from the base template.")
        (cvt:block (@ (name "e")) (p "Block E from the base template.")))))
 
  ((a () () (cvt:block (@ (name "a")) (p "Block A from extension template 1.")))
   (c ()
      ((cvt:defvar (@ (name "copyrightStatement")) "Copyright © " (cvt:var (@ (name "copyrightYear"))) " by " (cvt:var (@ (name "copyrightHolder"))) ". " (cvt:var (@ (name "rightsStatement")))))
      (cvt:block (@ (name "c")) (p "Block C from extension template 2.")))
   (e ()
      ((cvt:defvar (@ (name "copyrightStatement")) "Copyright © " (cvt:var (@ (name "copyrightYear"))) " by " (cvt:var (@ (name "copyrightHolder"))) ". " (cvt:var (@ (name "rightsStatement")))))
      (cvt:block (@ (name "e")) "Block E from extension template 2."))))

'((*TOP*
    (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml") (cvt "http://xmlns.therebetygers.net/civet/0.1")))
    (html
      (@ (xml:lang "en") (lang "en")) (head (title))
      (body
        (cvt:block (@ (name "a")) (p "Block A from the base template."))
        (cvt:block (@ (name "b")) (p "Block B from the base template."))
        (cvt:block (@ (name "c")) (p "Block C from the base template."))
        (cvt:block (@ (name "d")) "\n      Block D from the base template.\n    ")
        (cvt:block (@ (name "e")) (p "Block E from the base template.")))))
  ((a () () (cvt:block (@ (name "a")) (p "Block A from extension template 1.")))
   (e () ((copyrightStatement cvt:defvar (@ (name "copyrightStatement")) "\n      Copyright © " (cvt:var (@ (name "copyrightYear"))) " by " (cvt:var (@ (name "copyrightHolder"))) ".\n      " (cvt:var (@ (name "rightsStatement"))))) (cvt:block (@ (name "e")) "\n    Block E from extension template 2.\n  "))
   (c () ((copyrightStatement cvt:defvar (@ (name "copyrightStatement")) "\n      Copyright © " (cvt:var (@ (name "copyrightYear"))) " by " (cvt:var (@ (name "copyrightHolder"))) ".\n      " (cvt:var (@ (name "rightsStatement"))))) (cvt:block (@ (name "c")) (p "Block C from extension template 2.")))))

'((*TOP*
    (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml") (cvt "http://xmlns.therebetygers.net/civet/0.1")))
    (html
      (@ (xml:lang "en") (lang "en")) (head (title))
      (body
        (cvt:block (@ (name "a")) (p "Block A from the base template."))
        (cvt:block (@ (name "b")) (p "Block B from the base template."))
        (cvt:block (@ (name "c")) (p "Block C from the base template."))
        (cvt:block (@ (name "d")) "Block D from the base template.")
        (cvt:block (@ (name "e")) (p "Block E from the base template.")))))
  ((a () () (cvt:block (@ (name "a")) (p "Block A from extension template 1.")))
   (c (cvt:locale (@ (lang "ja") (encoding "shift-jis") (country "jp"))) ((cvt:defvar (@ (name "copyrightStatement")) "\n      Copyright © " (cvt:var (@ (name "copyrightYear"))) " by " (cvt:var (@ (name "copyrightHolder"))) ".\n      " (cvt:var (@ (name "rightsStatement")))) (cvt:defvar (@ (value "hullo") (name "junk"))) (cvt:defvar (@ (name "articleURL")) (cvt:var (@ (name "urlScheme"))) "://" (cvt:var (@ (name "host"))) "/articles/" (cvt:var (@ (name "articleID"))))) (cvt:block (@ (name "c")) (p "Block C from extension template 2.")))
   (e (cvt:locale (@ (lang "ja") (encoding "shift-jis") (country "jp"))) ((cvt:defvar (@ (name "copyrightStatement")) "\n      Copyright © " (cvt:var (@ (name "copyrightYear"))) " by " (cvt:var (@ (name "copyrightHolder"))) ".\n      " (cvt:var (@ (name "rightsStatement")))) (cvt:defvar (@ (value "hullo") (name "junk"))) (cvt:defvar (@ (name "articleURL")) (cvt:var (@ (name "urlScheme"))) "://" (cvt:var (@ (name "host"))) "/articles/" (cvt:var (@ (name "articleID"))))) (cvt:block (@ (name "e")) (a (@ (cvt:href "articleURL")) "Check out this great article!")))))

'((*TOP*
    (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml") (cvt "http://xmlns.therebetygers.net/civet/0.1")))
    (html
      (@ (xml:lang "en") (lang "en")) (head (title))
      (body
        (cvt:block (@ (name "a")) (p "Block A from the base template."))
        (cvt:block (@ (name "b")) (p "Block B from the base template."))
        (cvt:block (@ (name "c")) (p "Block C from the base template."))
        (cvt:block (@ (name "d")) "\n      Block D from the base template.\n    ")
        (cvt:block (@ (name "e")) (p "Block E from the base template.")))))
  ((a () () (cvt:block (@ (name "a")) (p "Block A from extension template 1.")))
   (e () ((copyrightStatement cvt:defvar (@ (name "copyrightStatement")) "\n      Copyright © " (cvt:var (@ (name "copyrightYear"))) " by " (cvt:var (@ (name "copyrightHolder"))) ".\n      " (cvt:var (@ (name "rightsStatement")))) (junk cvt:defvar (@ (value "hullo") (name "junk"))) (articleURL cvt:defvar (@ (name "articleURL")) (cvt:var (@ (name "urlScheme"))) "://" (cvt:var (@ (name "host"))) "/articles/" (cvt:var (@ (name "articleID"))))) (cvt:block (@ (name "e")) (a (@ (cvt:href "articleURL")) "Check out this great article!")))
   (c () ((copyrightStatement cvt:defvar (@ (name "copyrightStatement")) "\n      Copyright © " (cvt:var (@ (name "copyrightYear"))) " by " (cvt:var (@ (name "copyrightHolder"))) ".\n      " (cvt:var (@ (name "rightsStatement")))) (junk cvt:defvar (@ (value "hullo") (name "junk"))) (articleURL cvt:defvar (@ (name "articleURL")) (cvt:var (@ (name "urlScheme"))) "://" (cvt:var (@ (name "host"))) "/articles/" (cvt:var (@ (name "articleID"))))) (cvt:block (@ (name "c")) (p "Block C from extension template 2.")))))

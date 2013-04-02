(use civet)

(define my-vars
    '((bodyClasses . "SingleArticle BlogPost") (htmlTitle . "Article Pagina")
      (jquerySrc . "/where/the/scripts/r")))

(define my-context (make-context vars: my-vars))

(render "base.html" my-context file: "base1.html")
; (pp (process-template-set "base.html" my-context))

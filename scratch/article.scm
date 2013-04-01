(use civet)

(define my-vars
  '((urlScheme . "http") (hostName . "blog.therebetygers.net")
    (articleID . "deadbeef") (jquerySrc . "/scripts/jquery.js")
    (bodyMD . "Lorem Ipsum bla bla bla.") (canEdit . "true")
    (bodyClasses . "SingleArticle BlogPost")
    (htmlTitle . "Article Pagina") (seriesTitle . "Legendary Cheeses")
    (partNo . "41") (articleTitle . "Gjetost")
    (articleSubTitle . "It's sweet-ish but not Swedish--it's Norwegian!")
    (articleBody . "Yes indeed, bla bla bla and so on.")
    (currentPath . "/road/to/nowhere")
    (copyrightYear . "2012-2013") (authorName . "A. Nonny Mouse")
    (rightsStatement . "All rights reserved.")))

(define my-context (make-context vars: my-vars))

(render "article.html" my-context file: "article1.html")

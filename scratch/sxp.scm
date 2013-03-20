(define cav-ns-uri "http://xmlns.therebetygers.net/coq-au-vin/0.1")

(define cav-ns-prefix (make-parameter 'cav))

(define (default-nsmap)
  `((#f . "http://www.w3.org/1999/xhtml")
    (,(cav-ns-prefix) . ,cav-ns-uri)))

(define test-doc
  '(*TOP*
    (@ (*NAMESPACES* (#f "http://www.w3.org/1999/xhtml")
                     (cav "http://xmlns.therebetygers.net/coq-au-vin/0.1")))
    (cav:template
      (@ (extends "base.html"))
      (cav:locale
        (@ (lang "en") (encoding "utf-8") (country "US")))
      (cav:definitions
        (cav:defvar
          (@ (strip "all") (name "editURL"))
          (cav:var (@ (name "urlScheme")))
          "://"
          (cav:var (@ (name "hostName")))
          "/content/"
          (cav:var (@ (name "articleID")))))
      (cav:block
        (@ (name "pageScripts"))
        (script
          (@ (type "text/javascript"))
          "var dirty = false;
           var bodyMD = "
          (cav:var (@ (name "bodyMD")))
          ";
           var bodyDiv;
           var bodyHTML;
           var canEdit = "
          (cav:var (@ (type "boolean") (repr "javascript") (name "canEdit")))
          ";
  
           function showEditButton() {
           }
    
           function beginEditing() {
             bodyDiv.empty();
           }
  
           function discardChanges() {
           }

           function previewChanges() {
           }

           function submitChanges() {
           }

           function setupForAJAX() {
             $(\"#beginEditForm\").removeAttr(\"action\");
             $(\"#beginEditForm\").removeAttr(\"method\");
           }

           $(document).ready(function () {
             bodyDiv = $(\"#articleBody\");
             bodyHTML = bodyDiv.html();
             if (canEdit) {
               showEditButton();
             }
           }); "))
           (cav:block
             (@ (name "bodyClass"))
             "SingleArticle")
           (cav:block
             (@ (name "headerContent"))
             (h1 "S I N G L E   A R T I C L E   P A G E"))
           (cav:block
             (@ (name "leftSideContent"))
             (em "Left"))
           (cav:block
             (@ (name "mainContent"))
             (cav:if
               (@ (test "%defined:seriesTitle"))
               (h4
                 (@ (id "seriesTitle"))
                 (cav:var
                   (@ (name "seriesTitle")))
                 (cav:if
                   (@ (test "%defined:partNo"))
                   ", Part "
                   (cav:var (@ (name "partNo"))))))
             (h3
               (@ (id "articleTitle")))
             (cav:if
               (@ (test "%defined:subTitle"))
               (h5
                 (@ (id "articleSubTitle"))
                 (cav:var
                   (@ (name "articleSubTitle")))))
             (div
               (@ (id "articleBody"))
               (cav:var
                 (@ (name "articleBody"))))
             (form
               (@ (cav:attr "action=editURL") (method "POST") (id "beginEditForm"))
               (input
                 (@ (cav:attr "value=currentPath") (type "hidden")
                    (name "returnTo") (id "returnTo")))
               (button
                 (@ (type "submit") (name "editButton") (id "editButton"))
                 "Edit")))
           (cav:block
             (@ (name "rightSideContent"))
             (em "Right"))
           (cav:block
             (@ (name "footerContent"))
             (p
               (@ (class "copyrightNotice"))
               "Copyright © "
               (cav:var
                 (@ (name "copyrightYear")))
               " by "
               (cav:var
                 (@ (name "authorName")))
               ". "
               (cav:var
                 (@ (name "rightsStatement")))
               ".  ")))))


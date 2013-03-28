;;; civet.scm -- Implements a (somewhat) simple XML-based templating
;;;   system for the Coq au Vin blogging library.
;;;
;;;   Copyright © 2013 by Matthew C. Gushee <matt@gushee.net> 
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module civet
        (*site-path*
         *template-path*
         *template-cache-path*
         *enable-l10n*
         *template-vars*
         *template-blocks*
         *civet-ns-prefix*
         *civet-ns-uri*
         *default-nsmap*
         *sxpath-nsmap*
         *sort-functions*
         make-context
         context->context
         update-cached-template?
         load-template
         build-template-set
         process-base-template
         process-template-set
         render)
        (import
          (except scheme
            string-length string-ref string-set! make-string string substring
            string->list list->string string-fill! write-char read-char display)
          (except chicken
            reverse-list->string print print*))

        (include "civet-impl.scm")

) ; END MODULE

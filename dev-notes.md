Civet template processor: developer's notes
===========================================

This document is an informal record of problematic issues in 
civet, and in particular the rationales for doing or not doing
various things in the library.

- Is <cvt:locale> actually useful for anything? It seemed like a
  a good idea at the time: I thought, for example, it might be used
  to determine things like currency and date formatting. But on
  further consideration, I can't really come up with a use case for
  it. It seems to me that in general all locale-specific settings
  should be based on either site policy or end-user preferences.

- <cvt:with> is a rather awkward structure. Is there a cleaner
  way to create a container for local bindings?

- <cvt:interpolate> seems like big old kludge. I initially considered
  adding either elements like <cvt:on-last> or adding functions
  like last() to the expression language for <cvt:if>. The problem
  with either of those approaches is that they would require tracking
  a considerable amount of info about the state of the loop, whereas
  <cvt:interpolate> is processed in a top-down fashion--i.e. any
  <cvt:interpolate> children of a <cvt:for> are evaluated before 
  entering the loop. This solution is definitely simpler in terms
  of the Scheme library code, so I expect it should be faster and
  less error-prone. But I would like to do this in a more general
  way.

- The main processing algorithm is proving problematic for certain
  cases, such as implementing macros. The normal processing flow
  recurses through the document tree using **process-tree** -- a
  dispatch function which invokes specialized functions for various
  element types. However, there are a few elements that are handled
  as part of their parent elements, and skipped over when encountered
  by **process-tree**. The result of this is that, for example, the
  following will not work:

      <cvt:defmacro name="editHREF">
        <cvt:attr name="href"><cvt:var name="editURL"/></cvt:attr>
      </cvt:defmacro>
   
      ....

      <a>
        <cvt:macro name="editHREF"/>
        ....
      </a>

  If it isn't obvious, this is because the macro reference evaluates
  to the following:

      <a>
        <cvt:attr name="href"><cvt:var name="editURL"/></cvt:attr>
        ....
      </a>
  
  ... and since the <cvt:attr> element was unknown at the time the 
  handler for <a> was invoked, nothing is done for <cvt:attr>.

  Note that the following *does* work:

      <cvt:defmacro name="editHREF">
        <a>
          <cvt:attr name="href"><cvt:var name="editURL"/></cvt:attr>
        </a>
      </cvt:defmacro>
  
  But depending on the content of the <a> element, this can create
  more problems than it solves. 

  So I'm wondering if it is wise to skip any elements in the main flow. On
  the other hand, the alternative to 'top-down' processing for these
  problematic elements would seem to be to give elements the ability to
  access and modify their parents, which is obviously a complex undertaking.

- I'm concerned about template set structure. The current model looks
  something like this:

                 |----------------------------------------------------|
      EXTENSION: |  HEAD - - - - - - - - - - - - - - - -              |
                 |        \ -> BLOCK A                  \ -> BLOCK C  |
                 |-------------   |   -----------------------   |   --|
                                  |                             |
                 |-------------   |   -----------------------   |   --|
      BASE:      |  HEAD - - - -  V - - - - - - - - - - -       V     |
                 |        \ -> BLOCK A   \ -> BLOCK B    \ -> BLOCK C |
                 |----------------------------------------------------|

  I.e., blocks defined in an extension template override those defined in
  the base template. And in order to be available within any given block,
  variables and macros must be defined in the *same* template where the
  block is defined -- i.e. **defvars** and **defmacros** in the base
  template do not apply in the extension template. That doesn't seem quite
  right.

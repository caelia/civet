civet
=====

An XML-based templating system intended for use with the soon-to-be-released
blogging library Coq au Vin. The name comes from CVT, an abbreviation for
'Coq au Vin Templates'.


Basic Concepts
--------------

All Civet templates must be well-formed XML. Each template consists of
markup from the target document type (e.g., XHTML), which represents literal
output, combined with markup from the Civet template vocabulary, which
controls the structure of the output and inserts dynamic content.

A **template set** consists of a **base template** and one or more
**extension templates**. The base template determines the document type of
the output and its overall structure. As of Version 0.1, the base template
is the only one that should include content outside of **blocks**. Any
content outside of blocks in an extension template will be ignored. 

Extension templates are used to specialize various aspects of the base
template. They may also be chained so as to specialize other extension
templates. Each extension template is associated with its **parent** (i.e.,
the template that it extends) by means of the `extends` attribute on the
document element.

XML namespaces are used to distinguish the template vocabulary from literal
content. For best results, we suggest that the default namespace be that of
the target document type, while template vocabulary markup should use a
prefix. The namespace URI for the template vocabulary is

    http://xmlns.therebetygers.net/civet/<version_number>

For best results, please ensure that all templates in your installation use
the same prefix for this namespace. Here at Coq au Vin World Headquarters,
we use the prefix `cvt`.


Scheme API
----------

### PARAMETERS

#### [parameter] \*site-path\*

The root directory of your web site. The template path is automatically
calculated from this path unless you set `*template-path*`. If the value
of this parameter is `#f`, the processor assumes that the current working
directory is the site path; likewise, if you set a relative path, that path
is assumed to be relative to the current directory. Therefore it is a good
idea to set this parameter to an absolute path.

**Default:** `#f`

#### [parameter] \*template-path\*

The directory where templates are stored. If the parameter is set to
`#f`, the processor will look for templates in a `templates` subdirectory of
the site path. Storing templates in subdirectories of the template path is
not currently supported.

**Default:** `#f`

#### [parameter] \*template-cache-path\*

The directory where cached SXML templates are stored. If it is set to `#f`,
cached templates will be searched for in a `.cache` subdirectory of the
template path.

**Default:** `#f`

#### [parameter] \*enable-l10n\*

Enable localization? Currently has no effect.

**Default:** `#f`

#### [parameter] \*civet-ns-prefix\*

A symbol representing the prefix to be used for Civet vocabulary elements.
Do not override this unless you are actually using a different prefix in
your templates.

**Default:** `'cvt`

#### [parameter] \*civet-ns-uri\*

The namespace URI for Civet vocabulary elements. Overriding this is not
recommended.

**Default:** `"http://xmlns.therebetygers.net/civet/0.1"`

#### [parameter] \*default-nsmap\*

The default namespace map to be used when loading XML templates.

**Default:**

    `((#f . "http://www.w3.org/1999/xhtml")
      (,(*civet-ns-prefix*) . ,(*civet-ns-uri*)))))

#### [parameter] \*sxpath-nsmap\*

The default namespace map to be used for SXPath expressions and
serialization.

**Default:**

    `((*default* . "http://www.w3.org/1999/xhtml")
      (,(*civet-ns-prefix*) . ,(*civet-ns-uri*)))

#### [parameter] \*sort-functions\*

A mapping from data type symbols to sorting functions to be used in `for`
loops. For each "built-in" data type there are two functions specified: the
first is used for ascending sorts, the second for descending.

**Default:**

    `((string . (,string<? ,string>?))
      (char . (,char<? ,char>?))
      (number . (,< ,>))
      (boolean . (,(lambda (a b) (or (not a) b)) ,(lambda (a b) (or a (not b))))))

### PROCEDURES

#### [procedure]  `(render TEMPLATE CONTEXT #!key (PORT #f) (FILE #f))`

This is the main function used to transform a template to some form of
useful output (such as an XHTML web page). The TEMPLATE argument should be
the filename of a template, including the extension but not including the
directory path. CONTEXT is a context object (see `make-context` below for a
description). There are two keyword arguments to specify an output
destination: PORT is an output port, and FILE is a filename. If both are
given, PORT takes precedence; if neither is given, the output will be
returned as a string. 

#### [procedure]  `(process-template-set TEMPLATE CONTEXT)`

This function is similar to `render`, but returns an SXML document rather
than rendering to XML.

#### [procedure]  `(process-base-template TEMPLATE BLOCK-DATA CONTEXT)`

This function takes an SXML template, TEMPLATE, and an alist containing SXML
blocks (which generally will have been read from extension templates, but
could potentially be created programmatically), and a CONTEXT object, and
returns a transformed SXML document. The BLOCK-DATA alist consists of
`'((NAME . BLOCK) ...), where NAME corresponds to the name of a block in the
base template, and BLOCK is an SXML fragment consisting of a `cvt:block`
element and its content.

#### [procedure]  `(load-template NAME #!optional (NSMAP '())`

Loads a template from a file. NAME should be a filename including the
extension but excluding the directory path. The optional NSMAP argument is
an alist in the form `'((PREFIX . NAMESPACE-URI) ...)`, where PREFIX is a
symbol (except in the case of a default namespace, in which case it should
be `#f`. NSMAP overrides or extends the list of namespace bindings defined in
`*default-nsmap*`.

Note that when you load a template, a primitive form of caching is peformed
behind the scenes. Specifically, whenever you load a new or modified
template from XML, the resulting SXML document is saved to a file in the
cache path (`<site-path>/templates/.cache` by default). On subsequent
invocations of this procedure, if the cached SXML file exists and is newer,
it will be loaded in place of the XML file. At present it is not possible to
override this behavior.

#### [procedure]  `(build-template-set NAME #!optional (NSMAP '())`

Given the NAME of an template file and an optional NSMAP, this procedure
loads that template and any ancestors it references, and returns two values,
the base template and an alist (referred to elsewhere as BLOCK-DATA) of the
blocks extracted from any extension templates in the set.

#### [procedure]  `(make-context KWARGS)`

Returns a context object: a closure that is used to maintain state
information during the processing run. The following keywords are supported:

- **vars**      alist, default = '()

- **attrs**     alist, default = '()

- **nsmap**     alist, default = (*default-nsmap*)

- **locale**    alist, default = '()

- **blocks**    alist, default = '()

- **state**     symbol, default = 'init

Directly manipulating the context object is not recommended. In general you
should use `context->context` (see below). However, should you need to get
or set any values, the closure responds to the following messages:

- `(set-var! SYMBOL VALUE)

- `(update-vars! ALIST)
`
- `(set-vars! ALIST)

- `(get-var SYMBOL)

- `(get-vars)

- `(get-field OBJ-NAME FIELD-NAME)

- `(pfx->uri NAMESPACE-PREFIX)

- `(uri->pfx NAMESPACE-URI)

- `(set-ns! PREFIX URI)

- `(update-nsmap! ALIST)

- `(set-nsmap! ALIST)

- `(get-nsmap)

- `(set-attrs! ALIST)

- `(set-attr! SYMBOL VALUE)

- `(get-attrs)

- `(delete-attrs!)

- `(set-block! SYMBOL SXML-FRAGMENT) 

- `(get-block SYMBOL)

- `(get-blocks)

- `(set-locale! ALIST)

- `(set-lang! LANG-CODE)

- `(set-country! COUNTRY-CODE)

- `(set-encoding! ENCODING-NAME)

- `(set-date-format! DATE-FORMAT)

- `(get-locale)

- `(set-state! SYMBOL)

- `(get-state)


#### [procedure]  `(context->context CONTEXT KWARGS)`

Returns a new context object based on the existing one, with the same data
as the original except as modified by the KWARGS. The following keyword
arguments are supported.

- **+vars**    Updates or sets one or more variables. Takes an alist.
- **+attrs**   Updates or sets one or more attributes. Takes an alist.
- **+nsmap**   Updates or sets one or more namespace bindings. Takes an alist.
- **+locale**  Updates or sets one or more locale options. Takes an alist.
- **+blocks**  Updates or sets one or more template blocks. Takes an alist.
- **-vars**    Unsets one or more variables. Takes a list of symbols.
- **-attrs**   Unsets one or more attributes. Takes a list of symbols.
- **-nsmap**   Unsets one or more namespace bindings. Takes a list of symbols.
- **-locale**  Unsets one or more locale options. Takes a list of symbols.
- **-blocks**  Unsets one or more template blocks. Takes a list of symbols.
- **state**    Sets the state. Takes a symbol


Template Vocabulary, version 0.1
--------------------------------

The following describes the complete vocabulary of elements and attributes
represented by the `civet` namespace. At present there is no formal
specification or schema for the language, and this document may be regarded
as a normative reference.  Please report any language in this document that
you find ambiguous or insufficiently clear.

Also, each element description includes a **Contents** subsection,
describing what child nodes are required or allowed. However, for all
elements, unless otherwise noted, markup from the target vocabulary is
permitted within any element of the template vocabulary, and comments and
processing instructions are unrestricted.

Please note also, that while all elements described are handled by the
processor and should not cause errors, some (e.g. locale) do not actually do
anything.


### ELEMENTS

#### template

This is the document element for an extension template. As of Version 0.1, a
`template` element may only contain `config`, `locale`, `defvar`, and
`block` elements. Any other content will be discarded by the processor.

A base template does **not** use `template`; rather, its document element
should be the document element required by the target document type.

##### Context:

Occurs only as the document element of an extension template.

##### Content:

May contain, in the following order:

- **head** [optional]

- **block** [zero or more]

May not contain text nodes or any markup from the target vocabulary.

##### Attributes:

- **extends** [required] Contains a reference to the parent template,
  expressed as a system file path.

------------------------------------------------------------------------

#### head

Contains elements that set variables and/or configure processing behavior.

##### Context:

First child of the document element of a template.

##### Contents:

May contain, in any order:

- **locale** [optional]

- **defvar** [zero or more]


------------------------------------------------------------------------

#### locale

May be used to determine locale options within a template. Currently has no
effect. I'm not sure if this element should be supported or not. Certainly,
locale options are useful at the application level, but not necessarily
within a template, so this element will probably be discontinued if it does
not prove useful in the near future.

##### Context:

Within `head`.

##### Content:

Empty.

##### Attributes:

- **lang**

- **country**

- **encoding**

- **date-format**


------------------------------------------------------------------------

#### defvar

Sets a variable's value within its local scope (i.e. for the template if
contained in `head`, otherwise within the lexical scope of its parent
element).

##### Context:

Within a `head`, `block`, or `with`.

##### Content:

Any element other than `template`, `head`, or `block`

##### Attributes:

- **name** [required] The variable name to set.

- **type** [optional, default=auto] The value may be any known datatype, where
  *known* includes the built-in types and any types defined in the processing
  application. A value of *auto* means that the type will be `node-set` if
  this element contains any markup from the target vocabulary, otherwise
  `string`.

- **value** [optional] This attribute may be used instead of child nodes to
  specify the value, if the value consists of a single text node. 


------------------------------------------------------------------------

#### block

A *block* is the basic unit of document structure, and may contain any type
of content, or be empty. The order of blocks within the document (and of any
interspersed content outside of blocks) is determined by the base template,
and may not be altered by extension templates.

Each block has a required `name` attribute. Following the usual practice, its
value must be unique within any given document. Furthermore, any block ID
used in an extension template must match one defined in the base template of
the set.

Nested blocks are not currently allowed, and will raise an error. Nesting
may be supported in future versions if it is deemed a useful feature and can
be implemented in a reasonable manner.

The following rules govern the relationships of corresponding blocks (i.e.,
those whose IDs match) among different templates in a set.

- *Defined with content in ancestor template, omitted in descendant:*

  Output includes the content defined in the ancestor template.

- *Defined as empty in ancestor template, omitted in descendant template:*

  No output.

- *Defined with content in ancestor template, defined as empty in descendant:*

  No output

- *Defined with content in ancestor template, and with different content in
  descendant:*

  Content defined in the descendant template replaces that defined in the
  ancestor template. The `super` element may be used to include the content
  from the ancestor.

##### Attributes:

- **name** [required] An arbitrary identifier that must be unique at document
  level.


------------------------------------------------------------------------

#### var

A placeholder for inserting dynamic content. Variables are generally passed
by the processor, but may also be defined within the template (see
`defvar`). Assignment is not supported.

##### Attributes:

- **name** [required] The identifier for the variable; in order to insert
  content, this value must be matched in the `vars` alist defined in the
  processing application, or by a variable defined within the template. If
  the name is a **qualified name**, indicated with dotted notation, the
  processor will retrieve the named field from the named object (i.e.:
  &lt;object&gt;.&lt;field&gt;)

- **required** [optional, default: true] Whether the variable is required to
  be defined. Any required variable that is undefined is an error.

- **type** [optional, default: string] A builtin or user-defined datatype.
  The builtin types are:

    + string

    + boolean

    + char

    + number

    + integer

    + float

    + list:&lt;type&gt;

    + object

    + node-list

- **format** [optional] The name of a formatting function defined in the
  civet library or in your application. This function is usually
  associated with a specific data type. Future versions of the library may
  support locale-dependent formatters, as for dates.


------------------------------------------------------------------------

#### if

The basic conditional structure. If the test specified by the `test`
attribute returns true, all content of the `if` element is written to the
output; otherwise it is ommitted. May contain an optional `else` element.

NOTE: The effects of nested `if` elements have not been tested.

##### Context:

May be contained within any element, but see above note on nesting.

##### Contents:

Any element except `template` and `block`.

##### Attributes:

- **test** [required] A boolean expression. See `Expression Language` below.

##### Expression Language:

The `test` attribute uses a simple expression language, including the
following expressions:

&lt;var-name&gt;   Returns #t if the variable is defined in the current
context, false otherwise.

&lt;var-name&gt; = &lt;expr&gt;   Returns #t if the variable value is equal
(using equal?) to the right-side expression. The right-side expression may
be a (quoted) string or numeric literal, or another variable name.

&lt;var-name&gt; != &lt;expr&gt;   Returns #t if the variable value is
unequal to the right-side expression.

&lt;function&gt;(&lt;var-name&gt;, &lt;expr&gt;)   Performs a numeric
comparison between the named variable and the right-side expression. Four
functions are supported:

- **lt**  Less than
- **gt**  Greater than
- **le**  Less than or equal to
- **ge**  Greater than or equal to

Whitespace is allowed but not required at the beginning and end of an
expression, and between any two tokens.

------------------------------------------------------------------------

#### else

The content of this element is output if the `if` test fails.


------------------------------------------------------------------------

#### for

Iterate over a list variable.

##### Attributes:

- **each** [required] Defines the local variable name for each iteration.

- **in** [required] Equivalent to the name attribute for `var` and `block`.

- **sort** [optional, default=auto] The sorting method to use. May be 'alpha',
  'numeric', 'auto', or the name of a user-defined procedure. 'Auto' means
  that civet will select a sorting method based on the datatype of the
  variable. This may degrade performance and produce unexpected results, so
  it is best to specify the sorting method whenever possible. 

- **sort-field** [optional] If the variable refers to a list of objects, this
  attribute specifies the field that should be used to sort the list.

- **order** [optional, default=asc] Possible values are 'asc' and 'desc'.

------------------------------------------------------------------------

#### with

A container for variable definitions.

##### Context:

Anywhere in a base template, or anywhere within a block in an extension
template.

##### Content:

Should contain one or more &lt;defvar&gt; elements [otherwise the `with`
element serves no purpose], followed by any other elements (except `block`
and `template`).

##### Attributes:

None.


------------------------------------------------------------------------

#### attr

Sets an attribute on its parent element. If the literal attribute is already
defined on the parent, the value specified by this element overrides it.
Otherwise, it adds a new attribute to the parent.

##### Context:

Any element from the target vocabulary.

##### Content:

A string, or an expression that evaluates to a string (such as a `var` element
with a string value).

##### Attributes:

- **name** [required]

- **type** [optional, default: string]

- **var**  [optional] Either this attribute or a `var` child element must be
  present.


### ATTRIBUTES

In addition to the attributes defined above for template vocabulary
elements, any uprefixed attribute from the target vocabulary may have the
`civet` namespace prefix applied to it. This indicates to the processor that
the attribute's value is a variable reference; when encountering such an
attribute, the processor will substitute the variable's value for the
reference and remove the prefix. Note that this method permits only the
substitution of a primitive data type with an obvious string representation.
If you require any more complex manipulations, such as converting lists or
objects to strings, or conditional processing, you must use the `attr`
element.

If an element has an attribute prefixed in this manner, and an `attr`
element child, the `attr` element overrides the prefixed attribute.


Template Processors
-------------------

A civet processor must fulfill the following requirements:

- It must support the entire template vocabulary as described in this
  document.

- If any templates in the input set are ill-formed or fail to conform to the
  requirements of this document, processing must end with an error.

- If template processing successfully completes, the output will be
  well-formed XML, with all markup from the template vocabulary removed.

Any failure with respect to meeting these requirements is a bug, and may be
reported as such. Please note, however, that since the template language
deliberately allows templates to include arbitrary fragments composed of
markup from non-civet vocabularies, it is impossible to guarantee the
validity of the output document.

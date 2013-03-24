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
is the only one that can include content outside of **blocks**.

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

##### Context:

First child of the document element of a template.

##### Contents:

May contain, in any order:

- **locale** [optional]

- **defvar** [zero or more]


------------------------------------------------------------------------

#### locale

##### Context:

Within `config`.

##### Content:

Empty.

##### Attributes:

- **lang**

- **country**

- **encoding**

- **date-format**


------------------------------------------------------------------------

#### defvar

##### Context:

Within a `head`, `block`, or `with`.

##### Content:

Any element other than `template`, `head`, or `block`

##### Attributes:

- **name** [required] 

- **type** [optional, default=auto] The value may be any known datatype,
  where *known* includes the built-in types and any types defined in the
  processing application. A value of *auto* means that the type will be
  `node-set` if this element contains any markup, otherwise `string`.

- **value** [optional] This attribute may be used instead of child nodes to
  specify the value, if the value consists of a single text node. 


------------------------------------------------------------------------

#### block

A *block* is the basic unit of document structure, and may contain any type
of content, or be empty. The order of blocks within the document (and of any
interspersed content outside of blocks) is determined by the base template,
and may not be altered by extension templates.

Each block has a required `id` attribute. Following the usual practice, its
value must be unique within any given document. Furthermore, any block ID
used in an extension template must match one defined in the base template of
the set.

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

- **id** [required] An arbitrary identifier that must be unique at document
  level.


------------------------------------------------------------------------

#### var

A placeholder for inserting dynamic content. Variables are generally passed
by the processor, but may also be defined within the template (see
`defvar`). Assignment is not supported.

##### Attributes:

- **name** [required] The identifier for the variable; in order to insert
  content, this value must be matched in the `vars` alist defined in the
  processing application, or by a variable defined within the template.

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

    + element

    + list:&lt;type&gt;

    + node-list

- **format** [optional] The name of a formatting function defined in the
  civet library or in your application. This function is usually
  associated with a specific data type. Future versions of the library may
  support locale-dependent formatters, as for dates.


------------------------------------------------------------------------

#### object


##### Attributes:

- **name** [required] Equivalent to the name attribute for `var` and `block`.


------------------------------------------------------------------------

#### field


------------------------------------------------------------------------

#### if


------------------------------------------------------------------------

#### else


------------------------------------------------------------------------

#### for-each

##### Attributes:

- **name** [required] Equivalent to the name attribute for `var` and `block`.

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


------------------------------------------------------------------------

#### attr

- **name** [required]

- **type** [optional, default: string]

- **var**  [optional] Either this attribute or a `var` child element must be
  present.


Template Processors
-------------------

A civet processor must fulfill the following requirements:

- It must support the entire template vocabulary as described in this
  document.

- If any templates in the input set are ill-formed or fail to conform to the
  requirements of this document, processing must end with an error.

- If template processing successfully completes, the output will be
  well-formed XML.

Any failure with respect to meeting these requirements is a bug, and may be
reported as such. Please note, however, that since the template language
deliberately allows templates to include arbitrary fragments composed of
markup from non-civet vocabularies, it is impossible to guarantee the
validity of the output document.

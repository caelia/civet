cav-templates
=============

An XML-based templating system intended for use with the soon-to-be-released
blogging library Coq au Vin.


Basic Concepts
--------------

All Coq au Vin templates must be well-formed XML. Each template consists of
markup from the target document type (e.g., XHTML), which represents literal
output, combined with markup from the Coq au Vin template vocabulary, which
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

    http://xmlns.therebetygers.net/coq-au-vin/<version_number>

For best results, please ensure that all templates in your installation use
the same prefix for this namespace. Here at Coq au Vin World Headquarters,
we use the prefix `cav`.



Template Vocabulary
-------------------

### ELEMENTS

#### template

This is the document element for a non-base template. As of Version 0.1, all
child nodes of a `template` element must be `block` elements. If any other
content is included, the behavior is undefined.

A base template does **not** use `template`; rather, its document element
should be the document element required by the target document type.

##### Attributes for `template`:

- **extends** [required] Contains a reference to the parent template, expressed
  as a system file path.


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

##### Attributes for `block`:

- **id** [required] An arbitrary identifier that must be unique at document
  level.


#### super

#### var

A placeholder for inserting dynamic content. Variables are generally passed
by the processor, but may also be defined within the template (see
`defvar`). Assignment is not supported.

##### Attributes for `var`:

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

    + node-list

- **format** [optional] The name of a formatting function defined in the
  cav-templates library or in your application. This function is usually
  associated with a specific data type. Future versions of the library may
  support locale-dependent formatters, as for dates.


#### list

This element is like `var`, but it defines a list of non-markup items to
insert. The inserted elements are separated by whitespace.

##### Attributes for `list`:

See `var`.


#### object


#### if


#### else


#### each


#### with


#### defvar


#### setattr


#### attr

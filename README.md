# XMLS

<center>Manual For Version 3.1</center>

## Summary

Xmls is a small, simple, non-validating xml parser for Common Lisp. It's designed to be a self-contained, easily embedded parser that recognizes a useful subset of the XML spec. It provides a simple mapping from xml to lisp structures or s-expressions and back.

Since XMLS was first released it has gained some additional complications/features. In particular:

*   **Now XMLS by default parses XML documents into lisp structures, rather than s-expressions.** This makes accessing the structures simpler and more reliable. See [section on backward compatibility](#Compatibility).
*   We have added clearly named accessors to further improve extraction of information from parsed XML.
*   Thanks to Max Rottenkolber, we now have the affiliated library, [`xmls/octets`](#octets) that will open streams for the XMLS parser, processing any content-type declarations in the process.

## Features

*   Free (BSD license).
*   Understands enough of the xml spec to parse many common documents, including those occurring in common internet protocols like xml-rpc, webdav, and BEEP. Parses 85 out of the 98 valid documents in the oasis parser compliance suite.
*   Small and easily embedded. The entire parser is contained in one file and it's currently less than 600 lines of code. Xmls is written in pure lisp and requires no external parsing tools or foreign libraries.
*   Supports xml namespaces.
*   Threadsafe.
*   Serializes s-expr list structures back to xml as well as parsing xml.

## Limitations

*   Parses entire document into memory and consequently can't handle large documents.
*   No detailed error reporting.
*   Hand-built LR parser, meaning the parser structure is a little hard to understand, and can be hard to modify. Use of CL-YACC or similar might be a preferable route for a rewrite.

## XML Representation

Parsed xml is represented as a nested lisp structure, unlike in the original version, where it was a lisp list. The s-expression representation is still maintained, and there are [functions to translate to and from this notation](#translators).

### XML representation as lisp structures

In the structure representation, a node, corresponding to an XML element, is defined as follows:

<pre>(defstruct (node (:constructor %make-node))
  name
  ns
  attrs
  children)</pre>

XMLS also includes a helper function, `make-node` for creating xml nodes of this form:

<pre>(make-node &key name ns attrs children)
</pre>

Xmls provides the corresponding accessor functions node-name, node-ns node-attrs, and node-children.

### XML representation as s-expressions

In the s-expression representation, a node is represented as follows:

<pre>(name (attributes) children*)
</pre>

A name is either a simple string, if the element does not belong to a namespace, or a list of (name namespace-url) if the element does belong to a namespace.

Attributes are stored as `(name value)` lists, with optional properties after the value for `(name value . plist)`.  At present, the only property used is `:attr-ns`, the namespace on the attribute, if any.  If there is no namespace, the attribute may not be present.

Children are stored as a list of either element nodes or text nodes.

For example, the following xml document:

```
<?xml version="1.0"?>
<!-- test document -->
<book title='The Cyberiad'>
  <!-- comment in here -->
  <author xmlns='http://authors'>Stanislaw Lem</author>
  <info:subject xmlns:info='http://bookinfo' rank='1'>&quot;Cybernetic Fables&quot;</info:subject>
</book>
```

Would parse as:

```
("book" (("title" "The Cyberiad"))
 (("author" . "http://authors") NIL "Stanislaw Lem")
 (("subject" . "http://bookinfo") (("rank" "1")) "\"Cybernetic Fables\""))
```

<a name="Compatibility">

### Backward Compatibility

</a>

To detect whether in this version of XMLS the return value of `PARSE` will be a list or a structure, check for the feature `:XMLS-NODES-ARE-STRUCTS`.

For old code that wants XML parsed into lists, instead of structures, you may replace calls to `(parse str)` with `(node->nodelist (parse str))`.

For greater convenience, we offer `PARSE-TO-LIST`, which performs the same function.

## Usage

The interface is straightforward. The two main functions are `PARSE` and `TOXML`.

<pre>(parse source &key (compress-whitespace t) (quash-errors t)
</pre>

Parse accepts either a string or an input stream (`source`) and attempts to parse the XML document contained therein. It will return the parse tree as a structure if it's successful or `nil` if parsing fails.

If `COMPRESS-WHITESPACE` is non-`NIL`, content nodes will be trimmed of whitespace and empty whitespace strings between nodes will be discarded.

<pre>(parse-to-list source (&rest args))
</pre>

Functions as `PARSE`, but returns a list representation of the XML document, instead of a structure.

<pre>(write-prologue xml-decl doctype stream)
</pre>

`write-prologue` writes the leading `<?xml ... ?>` and `<!DOCTYPE ... >` elements to `stream`. `xml-decl` is an alist of attribute name, value pairs. Valid xml-decl attributes per the xml spec are "version", "encoding", and "standalone", though `write-prologue` does not verify this. `doctype` is a string containing the document type definition.

<pre>(write-prolog xml-decl doctype stream)
</pre>

U.S. spelling alternative to `write-prologue`.

<pre>(write-xml xml stream &key (indent nil))
</pre>

`write-xml` accepts a lisp list in the format described above and writes the equivalent xml string to stream. Currently, if nodes use namespaces XMLS will not assign namespaces prefixes but will explicitly assign the namespace to each node. This will be changed in a later release. XMLS will indent the generated xml output if `indent` is non-nil.

<pre>(toxml node &key (indent nil))
</pre>

`TOXML` is a convenience wrapper around `write-xml` that returns the in a newly allocated string.

<a name="translators">

### Translating to and from s-expressions

</a>

XMLS provides two exported functions to translate between the CL structure representation of the XML tree and the s-expression representation:

<dl>

<dt>`node->nodelist`</dt>

<dd>Translate the structure representation into s-expressions.</dd>

<dt>`nodelist->nodes`</dt>

<dd>Translate the s-expression representation of an XMLS parse tree into lisp structures.</dd>

</dl>

### Helper functions

These are intended to allow programmers to avoid direct manipulation of the XMLS element representation. If you use these, your code should be easier to read and you will avoid problems if there is a change in internal representation (such changes would be hard to even find, much less correct, if using the lists directly).

<dl>

<dt>`make-xmlrep (tag &key attribs children)`</dt>

<dd>Constructor function.</dd>

<dt>`xmlrep-add-child! (xmlrep child)`</dt>

<dd>Add a new child node to the XMLREP node.</dd>

<dt>`xmlrep-tag (xmlrep)`</dt>

<dd>Extract the tag from XMLREP.</dd>

<dt>`xmlrep-tagmatch (tag treenode)`</dt>

<dd>Returns true if TAG is the tag of TREENODE. Match is case _insensitive_ (quite possibly this is the Wrong Thing).</dd>

<dt>`xmlrep-attribs (xmlrep)`</dt>

<dd>Extract the attributes from an XMLREP node.</dd>

<dt>`xmlrep-children (xmlrep)`</dt>

<dd>Extract the children from an XMLREP node.</dd>

<dt>`xmlrep-find-child-tags (tag treenode)`</dt>

<dd>Return all of the (direct) children of TREENODE whose tags are TAG. Matching done by [`xmlrep-tagmatch`](#xmlrep-tagmatch).</dd>

<dt>`xmlrep-find-child-tag (tag treenode &optional (if-unfound :error))`</dt>

<dd>Find a _single_ child of TREENODE with TAG. Returns an error if there is more or less than one such child.</dd>

<dt>`xmlrep-string-child (treenode &optional (if-unfound :error))`</dt>

<dd>Returns the _single_ string-valued child of TREENODE. If there is more than one child, or if a single child is not a simple value, returns IF-UNFOUND, which defaults to :ERROR.</dd>

<dt>`xmlrep-integer-child (treenode)`</dt>

<dd>Find the _single_ child of TREENODE whose value is a string that can be parsed into an integer. Returns an error if there is more than one child, or if a single child is not appropriately valued.</dd>

<dt>`xmlrep-attrib-value (attrib treenode &optional (if-undefined :error))`</dt>

<dd>Find the value of ATTRIB, a string, in TREENODE. if there is no ATTRIB, will return the value of IF-UNDEFINED, which defaults to :ERROR.</dd>

<dt>`xmlrep-boolean-attrib-value (attrib treenode &optional (if-undefined :error))`</dt>

<dd>Find the value of ATTRIB, a string, in TREENODE. The value should be either "true" or "false". The function will return T or NIL, accordingly. If there is no ATTRIB, will return the value of IF-UNDEFINED, which defaults to :ERROR.</dd>

</dl>

<a name="octets">

## XMLS/Octets

</a>

XMLS itself simply processes strings or streams. This means that it does not provide native support for handling character encodings, as declared in the XML headers. The system `xmls/octets`, which depends on `xmls` provides that support with the exported function `make-xml-stream`, which takes an octet-stream as argument, processes its header, choosing the appropriate character encoding, and then returns a stream suitable for passing to `xmls:parse`.

Probably `make-xml-stream` should be made generic, and support arguments of other types (e.g., strings interpreted as filenames, pathnames, etc.).

## Installation

XMLS can be installed as an ASDF system. An ASDF system definition is provided with the distribution.

Previous versions of XMLS were single files, and could be installed simply by loading the file xmls.lisp. This option is no longer supported.

## Contact Information

Please post issues in the [GitHub Repository](https://github.com/rpgoldman/xmls/issues)

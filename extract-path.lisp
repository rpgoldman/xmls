;; (declaim (optimize (speed 0) (space 0) (debug 3) (safety 3) (compilation-speed 0)))

(in-package :xmls)

;; XML extraction tool

(defun extract-path ( key-list xml )
  "Extracts data from XML parse tree.  KEY-LIST is a path for descending down named
objects in the argument XML.  For each path element, enclosed XML subforms are
searched for a matching name.  Finally the whole last XML subform on the path is
normally returned; however the symbol * may be added at the end of KEY-LIST to return
list of all objects enclosed by the last subform on the path. KEY-LIST may be dotted
as explained below to return XML tag attributes from the last subform on the path.

XML has the parse form as returned by XMLS:PARSE-TO-LIST or XMLS:NODE->NODELIST:
        (tag-name (attributes-list) subform*)

The first element in KEY-LIST must match the top level form in the XML.  Subsequently
each element in the KEY-LIST is to match the name of a subform.

An element of KEY-LIST may be a string atom.  In that case the first subform with
tag name matching the string is matched.  An element of KEY-LIST may also be a list of
string atoms in this format:
        (tag-name (attribute-name attribute-value))

The first subform with name matching TAG-NAME and having an attribute matching
ATTRIBUTE-NAME with value matching ATTRIBUTE-VALUE is matched.

Normally the whole subform matching last element in KEY-LIST is returned.  The symbol
* can be the last element of KEY-LIST to return list of all subforms enclosed by last
matched form.  Attributes of last matched subform may be searched by ending KEY-LIST
in dot notation, in which case the string after dot matches an attribute name.  The
two element list of attribute name and value is returned.  * may be used after dot to
return the whole attribute list.

In the case where the search fails NIL is returned.  However it is possible that the
search partially succeeds down the key path.  Three values are returned altogether
and the state of the 2nd and 3rd values gives information about how much of KEY-LIST
was matched, and at what point in XML:
        (values RESULT  KEY-LIST-FRAGMENT  XML-FRAGMENT)

When RESULT is non-NIL, the others are NIL. When result is NIL however, the others are:
        XML-FRAGMENT
          The last XML form that /did/ match in the key list.  It matches the first
          element of KEY-LIST-FRAGMENT.

        KEY-LIST-FRAGMENT
          The /remaining/ part of the KEY-LIST that did not succeed.  However the
          /first/ item on KEY-LIST-FRAGMENT matches the XML-FRAGMENT returned.  The
          failure is at the second item on KEY-LIST-FRAGMENT.

In the case of complete failure, where even the very first item on KEY-LIST does not
match the top XML form given, all three return values are NIL.  (It suffices to check
the first two return values.)"
  (labels ((find-test ( key xml-form )
             ;; test whether the XML-FORM matches KEY
             (cond
               ;; just the XML tag name in key
               ;; XML name is simple string
               ((and (stringp key)
                     (stringp (xmlrep-tag xml-form)))
                (string-equal key (xmlrep-tag xml-form)))

               ;; key form (tag-name (attr-name attr-value))
               (t
                (and (find-test (car key) xml-form)
                     (find (cadr key) (xmlrep-attribs xml-form) :test #'equal)))))

           (descend ( key-list xml-form )
             ;; recursive run down KEY-LIST.  If XML-FORM runs down to NIL before reaching
             ;; the end of KEY-LIST, it will be NIL at the end.  If not, what is
             ;; remaining of XML-FORM is the found item.
             (cond
               ;; KEY-LIST ends without dotted item, at the target XML form
               ((null (cdr key-list))
                (values xml-form nil nil))

               ;; dotted item at the end of KEY-LIST, search attribute list of target XML form
               ((atom (cdr key-list))
                (if (eq '* (cdr key-list))
                    (values (xmlrep-attribs xml-form) nil nil)
                    (find (cdr key-list)  (xmlrep-attribs xml-form)
                          :test (lambda (key item) (equal key (car item))))))

               ;; more tag names to match on KEY-LIST
               ('t
                (if (eq '* (cadr key-list))
                    (values (xmlrep-children xml-form) nil nil)
                    (let ((selected-xml-form (find (cadr key-list)  (xmlrep-children xml-form)
                                                   :test #'find-test)))
                      (if selected-xml-form
                          (descend (cdr key-list) selected-xml-form)

                          ;; no matching sub-form, indicate what part of KEY-LIST did not match
                          (values nil key-list xml-form))))))))

    ;; empty list, degenerate usage
    (when (null key-list)
      (error "KEY-LIST is empty."))

    ;; search down after initial match
    (if (find-test (car key-list) xml)
        (descend  key-list xml)
        (values nil nil nil))))

;;; $Id$
;;; xmls
;;; a simple xml parser for common lisp
;;; author: Miles Egan <miles@caddr.com>
;;; see COPYING file for license information

(defpackage xmls
  (:use :cl) ; :cl-user
  (:shadow #:read-char #:unread-char)
  (:export node-name node-ns node-attrs node-children make-node parse toxml write-xml
           node-p nodelist->node
           node->nodelist
           node ; needed to support use in typep

           ;; backwards compatibility
           #:parse-to-list

           ;; processing instruction objects
           proc-inst-p
           proc-inst-target
           proc-inst-contents

           write-prologue
           write-prolog
           ;; additional helpers from Robert P. Goldman
           make-xmlrep xmlrep-add-child!
           xmlrep-tag xmlrep-tagmatch
           xmlrep-attribs xmlrep-children
           xmlrep-string-child xmlrep-integer-child
           xmlrep-find-child-tags xmlrep-find-child-tag
           xmlrep-attrib-value
           xmlrep-boolean-attrib-value

           ;; tree searching from Daniel Eliason
           extract-path-list
           extract-path

           ;;debugging
           debug-on debug-off))


(in-package :xmls)

;;;-----------------------------------------------------------------------------
;;; GLOBAL SETTINGS
;;;-----------------------------------------------------------------------------
(defvar *strip-comments* t)
(defvar *compress-whitespace* t)
(defvar *test-verbose* nil)
(defvar *discard-processing-instructions*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (type vector *entities*))
  (defvar *entities*
    #(("lt;" #\<)
      ("gt;" #\>)
      ("amp;" #\&)
      ("apos;" #\')
      ("quot;" #\")))
  (defvar *whitespace* (remove-duplicates
                        '(#\Newline #\Space #\Tab #\Return #\Linefeed))))
(defvar *char-escapes*
  (let ((table (make-array 256 :element-type 'string :initial-element "")))
    (loop
     for code from 0 to 255
     for char = (code-char code)
     for entity = (first (find char *entities* :test #'char= :key #'second))
     do (setf (svref table code)
              (cond
                (entity
                 (concatenate 'string "&" entity))
                ((and (or (< code 32) (> code 126))
                      (not (= code 10))
                      (not (= code 9)))
                 (format nil "&#x~x;" code))
                (t
                 (format nil "~x" char))))
     finally (return table))
    table))

;;;---------------------------------------------------------------------------
;;; DYNAMIC VARIABLES
;;;---------------------------------------------------------------------------
(defvar *parser-stream* nil
  "The currently-being-parsed stream. Used so that we can appropriately track
the line number.")
(defvar *parser-line-number* nil)



;;;-----------------------------------------------------------------------------
;;; CONDITIONS
;;;-----------------------------------------------------------------------------
(define-condition xml-parse-error (error)
  ((line :initarg :line
         :initform nil
         :reader error-line))
  (:report (lambda (xpe stream)
             (format stream "XML-PARSE-ERROR~@[ at line ~d~]"
                     (error-line xpe)))))

(defmethod initialize-instance :after ((obj xml-parse-error) &key)
  (unless (slot-value obj 'line)
    (when *parser-line-number*
      (setf (slot-value obj 'line) *parser-line-number*))))

;;;-----------------------------------------------------------------------------
;;; NODE INTERFACE
;;;-----------------------------------------------------------------------------
(defstruct (node (:constructor %make-node))
  name
  ns
  attrs
  children)
  
(defun make-node (&key name ns attrs child children)
  "Convenience function for creating a new xml node."
  (when (and child children)
    (error "Cannot specify both :child and :children for MAKE-NODE."))
  (let ((children (if child
                      (list child)
                    children)))
    (%make-node :name name :ns ns
                :children children
                :attrs attrs)))

(defun nodelist->node (nodelist)
  "Take old-style list representation of XMLS nodes and translate it
into a NODE."
  (if (stringp nodelist)
      ;; child is a string -- a literal child
      ;; FIXME: is that the right XML jargon?
      nodelist
      (make-node :name (if (consp (car nodelist))
                           (caar nodelist)
                           (car nodelist))
                 :ns (if (consp (car nodelist))
                         (cdar nodelist)
                         nil)
                 :children (mapcar 'nodelist->node (cddr nodelist))
                 :attrs (second nodelist))))

(defun node->nodelist (node)
  "Backwards compatibility function.  Will take a NODE \(the output of
PARSE\), and translate it into a list structure that looks like the
output of PARSE from XMLS 1.x.  This should only be needed if there's
client code that didn't obey the API and instead directly accessed the
Lisp list structures that XMLS used to produce. Such code should be
fixed."
  (etypecase node
      (string node)
      (node
       (list* (if (node-ns node) (cons (node-name node) (node-ns node))
                  (node-name node))
              (node-attrs node)
              (mapcar 'node->nodelist (node-children node))))))
  

;;;-----------------------------------------------------------------------------

;;;---------------------------------------------------------------------------
;;; XML Processing Instruction
;;;---------------------------------------------------------------------------
(defstruct proc-inst
  (target "" :type string)
  (contents "" :type string)
  )


;;;-----------------------------------------------------------------------------
;;; UTILITY FUNCTIONS
;;;-----------------------------------------------------------------------------
(defun compress-whitespace (str)
  (if *compress-whitespace*
      (progn
        (setf str (string-trim *whitespace* str))
        (if (= 0 (length str))
            nil
            str))
      str))

;;;---------------------------------------------------------------------------
;;; Replicated the Norvig debug to avoid annoying dependency
;;;---------------------------------------------------------------------------
(defvar *dbg-ids* nil "Identifiers used by dbg")
(defmacro dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  `(when (member ',id *dbg-ids*)
     (fresh-line *debug-io*)
     (funcall #'format *debug-io* ,format-string ,@args)))
(defun debug-on (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun debug-off (&rest ids)
  "Stop dbg on the ids.  With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids) nil
                      (set-difference *dbg-ids* ids))))



;; (defun write-escaped (string stream)
;;   "Writes string to stream with all character entities escaped."
;;   #-allegro (coerce string 'simple-base-string)
;;   (when (eq stream t) (setf stream *standard-output*))
;;   (loop for char across string
;;         for esc = (svref *char-escapes* (char-code char))
;;         do (write-sequence esc stream)))

;;; Alternative definition, lifted from Edi Weitz's hunchentoot, per Norman
;;; Werner's suggestion. [2010/12/09:rpg]
(defun write-escaped (string stream)
  (write-string (escape-for-html string) stream))

(defun escape-for-html (string)
  "Escapes the characters #\\<, #\\>, #\\', #\\\", and #\\& for HTML output."
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
            while char
            do (case char
                 ((#\<) (write-string "&lt;" out))
                 ((#\>) (write-string "&gt;" out))
                 ((#\") (write-string "&quot;" out))
                 ((#\') (write-string "&#039;" out))
                 ((#\&) (write-string "&amp;" out))
                 (otherwise (write-char char out)))))))

(defun make-extendable-string (&optional (size 10))
  "Creates an adjustable string with a fill pointer."
  (make-array size
              :element-type 'character
              :adjustable t
              :fill-pointer 0))

(defun push-string (c string)
  "Shorthand function for adding characters to an extendable string."
  (vector-push-extend c string))

(defun translate-raw-value (raw-value)
  "Helper function for xml generation."
  (etypecase raw-value
    (string raw-value)
    (symbol (symbol-name raw-value))
    (integer (format nil "~D" raw-value))
    (float (format nil "~G" raw-value))))

(defun generate-xml (e s indent)
  "Renders a lisp node tree to an xml string stream."
  (if (> indent 0) (incf indent))
  (etypecase e
    (node
     (progn
       (dotimes (i (* 2 (- indent 2)))
         (write-char #\Space s))
       (format s "<~A~@[ xmlns=\"~A\"~]" (node-name e) (node-ns e))
       (loop for a in (node-attrs e)
             do (progn
                  (write-char #\Space s)
                  (write-string (first a) s)
                  (write-char #\= s)
                  (write-char #\" s)
                  (write-escaped (translate-raw-value (second a)) s)
                  (write-char #\" s))))
     (if (null (node-children e))
         (progn
           (write-string "/>" s)
           (if (> indent 0) (write-char #\Newline s)))
         (progn
           (write-char #\> s)
           (if (> indent 0) (write-char #\Newline s))
           (mapc #'(lambda (c) (generate-xml c s indent)) (node-children e))
           (if (> indent 0)
               (progn
                 (dotimes (i (* 2 (- indent 2)))
                   (write-char #\Space s))))
           (format s "</~A>" (node-name e))
           (if (> indent 0) (write-char #\Newline s)))))
    (number
     (generate-xml (translate-raw-value e) s indent))
    (symbol
     (generate-xml (translate-raw-value e) s indent))
    (string
     (progn
       (if (> indent 0)
           (progn
             (dotimes (i (* 2 (- indent 2)))
               (write-char #\Space s))))
       (write-escaped e s)
       (if (> indent 0) (write-char #\Newline s))))))

;;;-----------------------------------------------------------------------------
;;; PARSER STATE & LOOKAHEAD
;;;-----------------------------------------------------------------------------
(defstruct state
  "Represents parser state.  Passed among rules to avoid threading issues."
  (got-doctype nil)
  (lines 1 :type integer)
  nsstack
  stream)

(defun resolve-entity (ent)
  "Resolves the xml entity ENT to a character.  Numeric entities are
converted using CODE-CHAR, which only works in implementations that
internally encode strings in US-ASCII, ISO-8859-1 or UCS."
  (declare (type simple-string ent))
  (or (and (>= (length ent) 2)
           (char= (char ent 0) #\#)
           (code-char
            (if (char= (char ent 1) #\x)
                (parse-integer ent :start 2 :end (- (length ent) 1) :radix 16)
                (parse-integer ent :start 1 :end (- (length ent) 1)))))
      (second (find ent *entities* :test #'string= :key #'first))
      (error "Unable to resolve entity ~S" ent)))

(declaim (inline peek-stream))
(defun peek-stream (stream)
  "Looks one character ahead in the input stream.  Serves as a potential hook for
character translation."
  (peek-char nil stream nil))

(defun read-stream (stream)
  "Reads a character from the stream, translating entities as it goes."
  (let ((c (read-char stream nil)))
    (if (and c (not (char= c #\&)))
        c
        (loop with ent = (make-extendable-string 5)
              for char = (read-char stream)
              do (push-string char ent)
              until (char= char #\;)
              finally (return (resolve-entity (coerce ent 'simple-string)))))))

;;;---------------------------------------------------------------------------
;;; Shadow READ-CHAR and UNREAD-CHAR so we can count lines while we parse...
;;;---------------------------------------------------------------------------
(defun read-char (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (let ((eof-p nil))
    (let ((c
            (catch 'char-return
              (handler-bind
                  ((end-of-file
                     #'(lambda (e)
                         (declare (ignore e))
                         (unless eof-error-p
                           (setf eof-p t)
                           (throw 'char-return eof-value)))))
                (common-lisp:read-char stream t nil recursive-p)))))
    (when (and (eq stream *parser-stream*)
               (not eof-p)
               (char= c #\newline))
      (incf *parser-line-number*))
    c)))

(defun unread-char (char &optional (stream *standard-input*))
  (when (char= char #\newline)
    (decf *parser-line-number*))
  (common-lisp:unread-char char stream))
    
;;;END shadowing--------------------------------------------------------------

(define-symbol-macro next-char (peek-stream (state-stream s)))

(defmacro eat ()
  "Consumes one character from the input stream."
  `(read-char (state-stream s)))

(defmacro puke (char)
  "The opposite of EAT."
  `(unread-char ,char (state-stream s)))

(defmacro match (&rest matchers)
  "Attempts to match the next input character with one of the supplied matchers."
  `(let ((c (peek-stream (state-stream s))))
    (and
     (or ,@(loop for m in matchers
                 collect (etypecase m
                           (standard-char `(char= ,m c))
                           (symbol `(,m c)))))
     ;; cheat here a little bit - eat entire char entity instead
     ;; of peeked char
     (read-stream (state-stream s)))))

(defmacro match-seq (&rest sequence)
  "Tries to match the supplied matchers in sequence with characters in the input stream."
  `(and ,@(loop for s in sequence
                collect `(match ,s))))

(defmacro match* (&rest sequence)
  "Matches any occurances of any of the supplied matchers."
  `(loop with data = (make-extendable-string 10)
    for c = (match ,@sequence)
    while c
    do (push-string c data)
    finally (return data)))

(defmacro match+ (&rest sequence)
  "Matches one or more occurances of any of the supplied matchers."
  `(and (peek ,@sequence)
    (match* ,@sequence)))

(defmacro peek (&rest matchers)
  "Looks ahead for an occurance of any of the supplied matchers."
  `(let ((c (peek-stream (state-stream s))))
    (or ,@(loop for m in matchers
                collect (etypecase m
                          (standard-char `(char= ,m c))
                          (symbol `(,m c)))))))

(defmacro must (&rest body)
  "Throws a parse error if the supplied forms do not succeed."
  `(or (progn ,@body)
    (error 'xml-parse-error)))

;;;-----------------------------------------------------------------------------
;;; PARSER INTERNAL FUNCTIONS
;;;-----------------------------------------------------------------------------
(defstruct element
  "Common return type of all rule functions."
  (type nil :type symbol)
  (val nil))

(defun resolve-namespace (elem env)
  "Maps the ns prefix to its associated url via the supplied ns env."
  (let ((ns (node-ns elem)))
    (dolist (e env)
      (let ((nsurl (assoc ns e :test #'string=)))
        (and nsurl
             (setf (node-ns elem) (cadr nsurl))
             (return ns))))))

;;;-----------------------------------------------------------------------------
;;; MATCH AND RULE BUILDING UTILITIES
;;;-----------------------------------------------------------------------------
(defmacro defmatch (name &rest body)
  "Match definition macro that provides a common lexical environment for matchers."
  `(defun ,name (c)
    ,@body))

(defmacro defrule (name &rest body)
  "Rule definition macro that provides a common lexical environment for rules."
  `(defun ,name (s)
    ,@body))

(defmacro matchfn (name)
  "Convenience macro for creating an anonymous function wrapper around a matcher macro."
  `(lambda (s) (match ,name)))

(defun none-or-more (s func)
  "Collects any matches of the supplied rule with the input stream."
  (declare (type function func))
  (let ((val (funcall func s)))
    (if val
        (multiple-value-bind (res nextval)
            (none-or-more s func)
          (values res (cons val nextval)))
        (values t nil))))

(defun one-or-more (s func)
  "Collects one or more matches of the supplied rule with the input stream."
  (declare (type function func))
  (let ((val (funcall func s)))
    (if val
        (multiple-value-bind (res nextval)
            (none-or-more s func)
          (declare (ignore res))
          (cons val nextval))
        nil)))

;;;-----------------------------------------------------------------------------
;;; MATCHERS
;;;-----------------------------------------------------------------------------
(defmatch digit ()
  (and c (digit-char-p c)))

(defmatch letter ()
  (and c (alpha-char-p c)))

;; Modified because *whitespace* is not defined at compile
;; time. [2004/08/31:rpg]
(defmatch ws-char ()
  (member c *whitespace*))
;;;  (case c
;;;    (#.*whitespace* t)
;;;    (t nil)))

(defmatch namechar ()
  (or
   (and c (alpha-char-p c))
   (and c (digit-char-p c))
   (case c
     ((#\. #\- #\_ #\:) t))))

(defmatch ncname-char ()
  (or
   (and c (alpha-char-p c))
   (and c (digit-char-p c))
   (case c
     ((#\. #\- #\_) t))))

(defmatch attr-text-dq ()
  (and c (not (member c (list #\< #\")))))

(defmatch attr-text-sq ()
  (and c (not (member c (list #\< #\')))))

(defmatch chardata ()
  (and c (not (char= c #\<))))

(defmatch comment-char ()
  (and c (not (eql c #\-))))

;;;-----------------------------------------------------------------------------
;;; RULES
;;;-----------------------------------------------------------------------------
(defrule ncname ()
  (and (peek letter #\_)
       (match+ ncname-char)))

(defrule qname ()
  (let (name suffix)
    (and
     (setf name (ncname s))
     (or
      (and
       (match #\:)
       (setf suffix (ncname s)))
      t))
    (values name suffix)))

(defrule attr-or-nsdecl ()
  (let (suffix name val)
    (and
     (setf (values name suffix) (qname s))
     (or
      (and
       (progn
         (match* ws-char)
         (match #\=))
       (or
        (and
         (progn
           (match* ws-char)
           (match #\"))
         (setf val (match* attr-text-dq))
         (match #\"))
        (and
         (progn
           (match* ws-char)
           (match #\'))
         (setf val (match* attr-text-sq))
         (match #\'))))
      t)
     (if (string= "xmlns" name)
	 (list 'nsdecl suffix val)
	 ;; If SUFFIX is true, then NAME is Prefix and SUFFIX is
	 ;; LocalPart.
	 (if suffix
	     (list 'attr suffix val :attr-ns name)
	     (list 'attr name val))))))

(defrule ws ()
  (and (match+ ws-char)
       (make-element :type 'whitespace :val nil)))

(defrule name ()
  (and
   (peek namechar #\_ #\:)
   (match* namechar)))

(defrule ws-attr-or-nsdecl ()
  (and
   (ws s)
   (attr-or-nsdecl s)))

(defrule start-tag ()
  (let (name suffix attrs nsdecls)
    (and
     (peek namechar)
     (setf (values name suffix) (qname s))
     (multiple-value-bind (res a)
         (none-or-more s #'ws-attr-or-nsdecl)
       (mapcar (lambda (x) (if (eq (car x) 'attr)
                               (push (cdr x) attrs)
                               (push (cdr x) nsdecls)))
               a)
       res)
     (or (ws s) t)
     (values
      (make-node
       :name (or suffix name)
       :ns (and suffix name)
       :attrs attrs)
      nsdecls))))

(defrule end-tag ()
  (let (name suffix)
    (and
     (match #\/)
     (setf (values name suffix) (qname s))
     (or (ws s) t)
     (match #\>)
     (make-element :type 'end-tag :val (or suffix name)))))

(defrule comment ()
  (and
   (match-seq #\! #\- #\-)
   (progn
     (loop until (match-seq #\- #\- #\>)
           do (eat))
     t)
   (make-element :type 'comment)))

;;; For the CDATA matching of ]]> I by hand generated an NFA, and then
;;; determinized it (also by hand).  Then I did a simpler thing of just pushing
;;; ALL the data onto the data string, and truncating it when done.
(defrule comment-or-cdata ()
  (and
   (peek #\!)
   (must (or (comment s)
             (and
              (match-seq #\[ #\C #\D #\A #\T #\A #\[)
              (loop with data = (make-extendable-string 50)
                    with state = 0
                    for char = (eat)
                    do (push-string char data)
                    do (case state
                         (0
                          (case char
                            (#\]
                             (dbg :cdata "State 0 Match #\], go to state {0,1} = 4.")
                             (setf state 4))
                            (otherwise
                             (dbg :cdata "State 0 Non-], go to (remain in) state 0."))))
                         (4 ; {0, 1}
                          (case char
                            (#\]
                             (dbg :cdata "State 4 {0, 1}, match ], go to state {0,1,2} = 5")
                             (setf state 5))
                            (otherwise
                             (dbg :cdata "State 4 {0, 1}, Non-], go to state 0.")
                             (setf state 0))))
                         (5 ; {0, 1, 2}
                          (case char
                            (#\]
                             (dbg :cdata "State 5 {0, 1, 2}, match ], stay in state 5."))
                            (#\>
                             (dbg :cdata "State 5 {0, 1, 2}, match >, finish match and go to state 3.")
                             (setf state 3))
                            (otherwise
                             (dbg :cdata "State 5 {0, 1, 2}, find neither ] nor >; go to state 0.")
                             (setf state 0))))
                         )
                    until (eql state 3)
                    finally (return (make-element
                                     :type 'cdata
                                     :val (coerce
                                           ;; rip the ]]> off the end of the data and return it...
                                           (subseq data 0 (- (fill-pointer data) 3))
                                           'simple-string)))))))))


(declaim (ftype function element))     ; forward decl for content rule
(defrule content ()
  (if (match #\<)
      (must (or (comment-or-cdata s)
                (processing-instruction s)
                (element s)
                (end-tag s)))
      (or (let (content)
            (and (setf content (match+ chardata))
                 (make-element :type 'data :val (compress-whitespace content)))))))

(defrule element ()
  (let (elem children nsdecls end-name)
    (and
     ;; parse front end of tag
     (multiple-value-bind (e n)
         (start-tag s)
       (setf elem e)
       (setf nsdecls n)
       e)
     ;; resolve namespaces *before* parsing children
     (if nsdecls (push nsdecls (state-nsstack s)) t)
     (or (if (or nsdecls (state-nsstack s))
             (resolve-namespace elem (state-nsstack s)))
         t)
     ;; parse end-tag and children
     (or
      (match-seq #\/ #\>)
      (and
       (match #\>)
       (loop for c = (content s)
             while c
             do (etypecase c
                  (element (case (element-type c)
                             (end-tag
                              (return (setf end-name (element-val c))))
                             ;; processing instructions may be discarded
                             (pi
                              (unless *discard-processing-instructions*
                                (when (element-val c)
                                  (push (element-val c) children))))
                             (t (if (element-val c)
                                    (push (element-val c) children)))))))
       (string= (node-name elem) end-name)))
     ;; package up new node
     (progn
       (setf (node-children elem) (nreverse children))
       (make-element :type 'elem :val elem)))))

(defrule processing-instruction ()
  (let (name contents)
    (and
     (match #\?)
     (setf name (name s))
     (not (string= name "xml"))
     ;; contents of a processing instruction can be arbitrary stuff, as long
     ;; as it doesn't contain ?>...
     (setf contents (pi-contents s))
     ;; if we get here, we have eaten ?> off the input in the course of
     ;; processing PI-CONTENTS
     (make-element :type 'pi :val (make-proc-inst :target name :contents contents)))))

(defrule pi-contents ()
  (loop with data = (make-extendable-string 50)
         with state = 0
         for char = (eat)
         do (push-string char data)
         do (ecase state
              (0
               (case char
                 (#\?
                  (dbg :pi-contents "State 0 Match #\?, go to state 1.")
                  (setf state 1))
                 (otherwise
                  (dbg :pi-contents "State 0 ~c, go to (remain in) state 0." char))))
              (1
               (case char
                 (#\>
                  (dbg :pi-contents "State 1 Match #\>, done.")
                  (setf state 2))
                 (otherwise
                  (dbg :pi-contents "State 1, ~c, do not match #\>, return to 0." char)
                  (setf state 0)))))
         until (eql state 2)
         finally (return (coerce
                          ;; rip the ?> off the end of the data and return it...
                          (subseq data 0 (max 0 (- (fill-pointer data) 2)))
                          'simple-string))))

(defrule xmldecl ()
    (let (name contents)
    (and
     (match #\?)
     (setf name (name s))
     (string= name "xml")
     (setf contents (none-or-more s #'ws-attr-or-nsdecl))
     (match-seq #\? #\>)
     (make-element :type 'xmldecl :val contents))))

(defrule comment-or-doctype ()
  ;; skip dtd - bail out to comment if it's a comment
  ;; only match doctype once
  (and
   (peek #\!)
   (or (comment s)
       (and (not (state-got-doctype s))
            (must (match-seq #\D #\O #\C #\T #\Y #\P #\E))
            (loop with level = 1
                  do (case (eat)
                       (#\> (decf level))
                       (#\< (incf level)))
                  until (eq level 0)
                  finally (return t))
            (setf (state-got-doctype s) t)
            (make-element :type 'doctype)))))

(defrule misc ()
  (or
   (ws s)
   (and (match #\<) (must (or (processing-instruction s)
                              (comment-or-doctype s)
                              (element s))))))

(defrule document ()
  (let (elem)
    (if (match #\<)
        (must (or (xmldecl s)
                  (comment-or-doctype s)
                  (setf elem (element s)))))
    ;; NOTE: I don't understand this: it seems to parse arbitrary crap
    (unless elem
      (loop for c = (misc s)
            while c
            do (cond ((eql (element-type c) 'elem)
                      (return (setf elem c)))
                     ((and (eql (element-type c) 'pi)
                           (not *discard-processing-instructions*))
                      (return (setf elem c))))))
                       
    (and elem (element-val elem))))

;;;-----------------------------------------------------------------------------
;;; PUBLIC INTERFACE
;;;-----------------------------------------------------------------------------
(defun write-xml (e s &key (indent nil))
  "Renders a lisp node tree to an xml stream.  Indents if indent is non-nil."
  (if (null s)
      (toxml e :indent indent)
    (generate-xml e s (if indent 1 0))))

(defun write-prologue (xml-decl doctype s)
  "Render the leading <?xml ... ?> and <!DOCTYPE ... > tags to an xml stream."
  (format s "<?xml")
  (dolist (attrib xml-decl)
    (format s " ~A=\"~A\"" (car attrib) (cdr attrib)))
  (format s " ?>~%")
  (when doctype
    (format s "<!DOCTYPE ~A>~%" doctype)))

(defun write-prolog (xml-decl doctype s)
  (write-prologue xml-decl doctype s))

(defun toxml (e &key (indent nil))
  "Renders a lisp node tree to an xml string."
  (with-output-to-string (s)
    (write-xml e s :indent indent)))

(defun parse (s &key (compress-whitespace t) (quash-errors t))
  "Parses the supplied stream or string into a lisp node tree."
  (let* ((*compress-whitespace* compress-whitespace)
         (*discard-processing-instructions* t)
         (stream
           (etypecase s
             (string (make-string-input-stream s))
             (stream s)))
         (*parser-stream* stream)
         (*parser-line-number* 1))
    (if quash-errors
        (handler-case
            (document (make-state :stream stream))
          (end-of-file () nil)
          (xml-parse-error () nil))
        (document (make-state :stream stream)))))

(defun parse-to-list (&rest args)
  (node->nodelist (apply #'parse args)))

(defparameter *test-files*
  (mapcar #'(lambda (x) (asdf:system-relative-pathname "xmls" (format nil "tests/~a" x)))
          (list "ant/build.xml"
                "beep/greeting1.xml"
                "beep/msg1.xml"
                "cdata/cdata1.xml"
                "char-encoding/flux-test-utf-8.xml"
                "dav/propfind1.xml"
                "dav/propfind2r.xml"
                "dav/propfind3.xml"
                "large/two_gent.xml"
                "misc/entity.xml"
                "misc/example.xml"
                "misc/minimal.xml"
                "misc/minimal1.xml"
                "misc/minimal2.xml"
                "misc/minimal3.xml"
                "misc/whitespace.xml"
                "namespace/namespace1.xml"
                "namespace/namespace2.xml"
                "namespace/namespace3.xml"
                "nxml/genetics-article.xml"
                "nxml/no-processing-instructions.xml"
                "nxml/processing-instructions.xml"
                "rss.xml"
                "soap/soap1.xml"
                "soap/soap2.xml"
                "soap/soap3.xml"
                "soap/soap4.xml"
                "soap/soap5.xml"
                "soap/soap6.xml"
                "xml-rpc/array.xml"
                "xml-rpc/blogger1.xml"
                "xml-rpc/fault.xml"
                "xml-rpc/methodCall.xml"
                "xml-rpc/methodResponse.xml"
                "xml-rpc/struct.xml")))

#+(or sbcl cmu allegro abcl ccl clisp ecl) 
(defun test (&key interactive (test-files *test-files*))
  "Run the test suite. If it fails, either return NIL \(if INTERACTIVE\),
otherwise exit with an error exit status."
  ;;(sb-profile:profile "XMLS")
  #+cmu(extensions:gc-off) ;; too noisy
  #+clisp (pprint ext:*args*)
  (let ((exit-code 0))
    (dolist (test
                (or test-files
                    #-(or ccl clisp)
                    (cdr
                     #+sbcl  (member "--" sb-ext:*posix-argv* :test 'equal)
                     #+abcl extensions:*command-line-argument-list*
                     #+cmu  (member "--" extensions:*command-line-strings* :test 'equal)
                     #+allegro (sys:command-line-arguments)
                     #+clisp ext:*args*
                     #+ecl (ext:command-args)
                     #+ccl
                     ccl:*unprocessed-command-line-arguments*)))
      (catch 'test-failure
        (handler-bind ((error #'(lambda (c)
                                    (format t "FAILED with error:~%~a~%" c)
                                    (setf exit-code 1)
                                    (throw 'test-failure nil))))
          (with-open-file (str test :direction :input)
            (if *test-verbose*
                (let ((parsed (parse str :compress-whitespace t)))
                  (if parsed
                      (format t "~A~%" (toxml parsed :indent t))
                      (progn
                        (format t "~&Failed to parse ~A~%" test)
                        (setf exit-code 1))))
                (progn
                  (format t "~40A" (concatenate 'string (namestring test) "... "))
                  (force-output)
                  (cond ((parse str)
                         (format t "ok~%"))
                        (t
                         (setf exit-code 1)
                         (format t "FAILED!~%"))))))
          (with-open-file (str test :direction :input)
           (if *test-verbose*
               (let ((parsed (parse-to-list str :compress-whitespace t)))
                 (if parsed
                     (format t "~A~%" (toxml parsed :indent t))
                     (progn
                       (format t "~&Failed to parse ~A~%" test)
                       (setf exit-code 1))))
               (progn
                 (format t "~40A" (concatenate 'string (namestring test) "... "))
                 (force-output)
                 (cond ((parse-to-list str)
                        (format t "ok~%"))
                       (t
                        (setf exit-code 1)
                        (format t "FAILED!~%")))))))))
    (catch 'test-failure
      (handler-bind ((error #'(lambda (c)
                                (format t "FAILED with error:~%~S~%" c)
                                (setf exit-code 1)
                                (throw 'test-failure nil))))
        (format t "~40A" "Escaped writing...")
        (force-output)
        (with-output-to-string (str)
          (write-escaped "ÄΩ" str))
        (format t "ok~%")))
    (if interactive
        (zerop exit-code)
        (uiop:quit exit-code))))

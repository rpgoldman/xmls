;;; $Id$

(defpackage xmls
  (:use :cl :cl-user)
  (:export node-name node-ns node-attrs node-children make-node parse toxml write-xml))

(in-package :xmls)

;;;-----------------------------------------------------------------------------
;;; GLOBAL SETTINGS
;;;-----------------------------------------------------------------------------
(defvar *strip-comments* t)
(defvar *compress-whitespace* t)
(defvar *test-verbose* nil)
(defvar *whitespace* (remove-duplicates '(#\Newline #\Space #\Tab #\Return #\Linefeed)))
(defvar *entities*
  #(("lt;" #\<)
    ("gt;" #\>)
    ("amp;" #\&)
    ("apos;" #\')
    ("quot;" #\")))

(defvar *char-escapes*
  (let ((table (make-array 256 :element-type 'string :initial-element "")))
    (declare (type vector *entities*))
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

;;;-----------------------------------------------------------------------------
;;; CONDITIONS
;;;-----------------------------------------------------------------------------
(define-condition xml-parse-error (error) 
  ((line :initarg :line
         :reader error-line)))

;;;-----------------------------------------------------------------------------
;;; NODE INTERFACE
;;;-----------------------------------------------------------------------------
(defun make-node (&key name ns attrs child children)
  "Convenience function for creating a new xml node."
  (list* (if ns (cons name ns) name) 
         attrs 
         (if child
             (list child)
             children)))

(defun node-name (elem)
  (if (consp (car elem))
      (caar elem)
      (car elem)))

(defun node-ns (elem)
  (if (consp (car elem))
      (cdar elem)
      nil))

(defun (setf node-ns) (ns elem)
  (setf (car elem)
        (cons (node-name elem) ns)))

(defun node-attrs (elem) (second elem))

(defun (setf node-attrs) (attrs elem) 
  (setf (second elem) attrs))

(defun node-children (elem)
  (cddr elem))

(defun (setf node-children) (children elem) 
  (rplacd (cdr elem) children)
  (node-children elem))

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
  
(defun write-escaped (string stream)
  "Writes string to stream with all character entities escaped."
  (coerce string 'simple-base-string)
  (loop for char across string
        for esc = (svref *char-escapes* (char-code char))
        do (write-sequence esc stream)))

(defun make-extendable-string (&optional (size 10))
  "Creates an adjustable string with a fill pointer."
  (make-array size
              :element-type 'character
              :adjustable t
              :fill-pointer 0))

(defun push-string (c string)
  "Shorthand function for adding characters to an extendable string."
  (vector-push-extend c string))

(defun generate-xml (e s indent)
  "Renders a lisp node tree to an xml string stream."
  (if (> indent 0) (incf indent))
  (typecase e
    (list
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
                  (write-escaped (second a) s)
                  (write-char #\" s))))
     (if (null (node-children e))
         (progn
           (write-string "/>" s)
           (if (> indent 0) (write-char #\Newline s)))
	 (progn
           (write-char #\> s)
           (if (> indent 0) (write-char #\Newline s))
           (mapcan (lambda (c) (generate-xml c s indent)) (node-children e))
           (if (> indent 0)
               (progn
                 (dotimes (i (* 2 (- indent 2)))
                   (write-char #\Space s))))
	   (format s "</~A>" (node-name e))
           (if (> indent 0) (write-char #\Newline s)))))
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
  (declare (type simple-base-string ent))
  (declare (type vector *entities*))
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

(define-symbol-macro next-char (peek-stream (state-stream s)))

(defmacro eat ()
  "Consumes one character from the input stream."
  `(read-char (state-stream s)))

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

(defmatch ws-char ()
  (case c
    (#.*whitespace* t)
    (t nil)))

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
     (if (string= "xmlns" name) (list 'nsdecl suffix val) (list
                                                           'attr (or suffix name) val)))))

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

(defrule comment-or-cdata ()
  (and
   (peek #\!)
   (must (or (comment s)
             (and
              (match-seq #\[ #\C #\D #\A #\T #\A #\[)
              (loop with data = (make-extendable-string 50)
                    with state = 0
                    do (case state
                         (0 (if (match #\])
                                (incf state)
                                (push-string (eat) data)))
                         (1 (if (match #\])
                                (incf state)
                                (progn 
                                  (setf state 0)
                                  (push-string #\] data)
                                  (push-string (eat) data))))
                         (2 (if (match #\>)
                                (incf state)
                                (progn 
                                  (setf state 0)
                                  (push-string #\] data)
                                  (push-string #\] data)
                                  (push-string (eat) data)))))
                    until (eq state 3)
                    finally (return (make-element 
                                     :type 'cdata 
                                     :val (coerce data 'simple-base-string)))))))))

(declaim (ftype function element))     ; forward decl for content rule
(defrule content ()
  (if (match #\<)
      (must (or (comment-or-cdata s)
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
                             ('end-tag
                              (return (setf end-name (element-val c))))
                             (t (if (element-val c)
                                    (push (element-val c) children)))))))
       (string= (node-name elem) end-name)))
     ;; package up new node
     (progn
       (setf (node-children elem) (nreverse children))
       (make-element :type 'elem :val elem)))))

(defrule processing-instruction-or-xmldecl ()
  (let (name)
    (and
     (match #\?)
     (setf name (name s))
     (none-or-more s #'ws-attr-or-nsdecl)
     (match-seq #\? #\>)
     (make-element :type 'pi :val name))))

(defrule processing-instruction ()
  (let ((p (processing-instruction-or-xmldecl s)))
    (and p
         (not (string= (element-val p) "xml"))
         p)))

(defrule xmldecl ()
  (let ((p (processing-instruction-or-xmldecl s)))
    (and p
         (string= (element-val p) "xml")
         p)))

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
        (must (or (processing-instruction-or-xmldecl s)
                  (comment-or-doctype s)
                  (setf elem (element s)))))
    (unless elem
      (loop for c = (misc s)
            while c do (if (eql (element-type c) 'elem)
                           (return (setf elem c)))))
    (and elem (element-val elem))))

;;;-----------------------------------------------------------------------------
;;; PUBLIC INTERFACE
;;;-----------------------------------------------------------------------------
(defun write-xml (e s &key (indent nil))
  "Renders a lisp node tree to an xml stream.  Indents if indent is non-nil."
  (generate-xml e s (if indent 1 0)))

(defun toxml (e &key (indent nil))
  "Renders a lisp node tree to an xml string."
  (with-output-to-string (s)
    (write-xml e s :indent indent)))

(defun parse (s &key (compress-whitespace t))
  "Parses the supplied stream or string into a lisp node tree."
  (let ((*compress-whitespace* compress-whitespace)
        (stream
         (etypecase s
           (string (make-string-input-stream s))
           (stream s))))
    (handler-case
        (document (make-state :stream stream))
      (end-of-file () nil)
      (xml-parse-error () nil))))

;;(trace end-tag comment comment-or-doctype content name xmldecl misc)
;;(trace processing-instruction processing-instruction-or-xmldecl element start-tag ws element-val)

#(or sbcl cmu allegro)
(defun test ()
  ;;(sb-profile:profile "XMLS")
  #+cmu(extensions:gc-off) ;; too noisy
  (dolist (test (cdr 
                 #+sbcl sb-ext:*posix-argv* 
                 #+cmu (subseq extensions:*command-line-strings* 4)
                 #+allegro (sys:command-line-arguments)))
    (if *test-verbose*
        (format t "~A~%" (toxml (parse (open test) :compress-whitespace t) :indent t))
        (progn
          (format t "~40A" (concatenate 'string test "... "))
          (if (parse (open test))
              (format t "ok~%")
              (format t "failed!~%")))))
  ;;(sb-profile:report)
  #+sbcl(sb-ext:quit)
  #+cmu(extensions:quit)
  #+allegro(excl:exit))

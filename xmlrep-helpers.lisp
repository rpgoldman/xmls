;;;---------------------------------------------------------------------------
;;; File Description:
;;;
;;;    Contains utility functions that are helpful in manipulating the
;;;    list representation that XMLS uses as the source or destination
;;;    of translation to or from XML.
;;;
;;; History/Bugs/Notes:
;;;
;;;   [2004/09/15:Robert P. Goldman] Created.
;;;
;;;---------------------------------------------------------------------------
;; (declaim (optimize (speed 0) (space 0) (debug 3) (safety 3) (compilation-speed 0)))

(in-package :xmls)

(defun make-xmlrep (tag &key (representation-kind :node) namespace attribs children)
  (case representation-kind
    ((:list)
     (cond
       (namespace
        (list (list tag namespace) (list attribs) children))
       (t
        (list tag (list attribs) children))))
    ((:node)
     (make-node :name tag :ns namespace :attrs attribs :children children))
    (otherwise
     (error "REPRESENTATION-KIND must be :LIST or :NODE, found ~s" representation-kind))))

(defgeneric xmlrep-add-child! (xmlrep child)
  (:method ((xmlrep node) child)
    (setf (node-children xmlrep)
          (append (node-children xmlrep)
                  (list child))))
  (:method ((xmlrep cons) child)
    (setf (cddr xmlrep)
          (append (cddr xmlrep)
                  (list child)))))

(defgeneric xmlrep-tag (treenode)
  (:method ((treenode node))
    (node-name treenode))
  (:method ((treenode cons))
    (let ((tag-name (car treenode)))
      ;; detect the "namespaced" case
      (cond
        ((consp tag-name) (car tag-name))
        (t tag-name)))))

(defun xmlrep-tagmatch (tag treenode)
  ;;child nodes to XMLREPs could be strings or nodes
  (unless (stringp treenode)
    (string-equal tag (xmlrep-tag treenode))))

(defgeneric xmlrep-attribs (treenode)
  (:method ((treenode node))
    (node-attrs treenode))
  (:method ((treenode cons))
    (cadr treenode)))

(defgeneric (setf xmlrep-attribs) (attribs treenode)
  (:argument-precedence-order treenode attribs)
  (:method (attribs (treenode node))
    (setf (node-attrs treenode) attribs))
  (:method (attribs (treenode cons))
    (setf (cadr treenode) attribs)))

(defgeneric xmlrep-children (treenode)
  (:method ((treenode node))
    (node-children treenode))
  (:method ((treenode cons))
    (cddr treenode)))

(defgeneric (setf xmlrep-children) (children treenode)
  (:argument-precedence-order treenode children)
  (:method (children (treenode node))
    (setf (node-children treenode) children))
  (:method (children (treenode cons))
    (setf (cddr treenode) children)))

(defun xmlrep-string-child (treenode &optional (if-unfound :error))
  (let ((children (xmlrep-children treenode)))
    (if (and (eq (length children) 1) (typep (first children) 'string))
        (first children)
        (if (eq if-unfound :error)
            (error "Node does not have a single string child: ~a" treenode)
            if-unfound)
        )))

(defun xmlrep-integer-child (treenode)
  (parse-integer (xmlrep-string-child treenode)))

(defun xmlrep-find-child-tags (tag treenode)
  "Find all the children of TREENODE with TAG."
  (remove-if-not #'(lambda (child) (xmlrep-tagmatch tag child))
                 (xmlrep-children treenode)))

(defun xmlrep-find-child-tag (tag treenode
                                  &optional (if-unfound :error))
  "Find a single child of TREENODE with TAG.  Returns an error
if there is more or less than one such child."
  (let ((matches (xmlrep-find-child-tags tag treenode)))
    (case (length matches)
      (0 (if (eq if-unfound :error)
             (error "Couldn't find child tag ~A in ~A"
                tag treenode)
             if-unfound))
      (1 (first matches))
      (otherwise (error "Child tag ~A multiply defined in ~A"
                        tag treenode)))))

(defun xmlrep-attrib-value (attrib treenode
                            &optional (if-undefined :error))
  "Find the value of ATTRIB, a string, in TREENODE.
if there is no ATTRIB, will return the value of IF-UNDEFINED,
which defaults to :ERROR."
  (let ((found-attrib (find-attrib attrib treenode)))
    (cond (found-attrib
           (second found-attrib))
          ((eq if-undefined :error)
           (error "XML attribute ~S undefined in ~S"
                  attrib treenode))
          (t
           if-undefined))))

(defun find-attrib (attrib treenode)
  "Returns the attrib CELL (not the attrib value) from 
TREENODE, if found.  This cell will be a list of length 2,
the attrib name (a string) and its value."
  (find attrib (xmlrep-attribs treenode)
        :test #'string=
        :key #'car))
  
(defun (setf xmlrep-attrib-value) (value attrib treenode)
  ;; ideally, we would check this...
  (let ((old-val (xmlrep-attrib-value attrib treenode nil)))
    (if old-val
        (cond ((null value)
               ;; just delete this attribute...
               (setf (xmlrep-attribs treenode)
                     (remove attrib (xmlrep-attribs treenode)
                             :test #'string=
                             :key #'first))
               nil)
              (t (let ((cell (find-attrib attrib treenode)))
                   (setf (second cell) value)
                   value)))
        ;; no old value
        (cond ((null value)
               nil)                         ; no old value to delete
              (t
               (setf (xmlrep-attribs treenode)
                     (append (xmlrep-attribs treenode)
                             (list (list attrib value))))
               value)))))

(defun xmlrep-boolean-attrib-value (attrib treenode
                                    &optional (if-undefined :error))
  "Find the value of ATTRIB, a string, in TREENODE.
The value should be either \"true\" or \"false\".  The
function will return T or NIL, accordingly.  If there is no ATTRIB,
will return the value of IF-UNDEFINED, which defaults to :ERROR."
  (let ((val (xmlrep-attrib-value attrib treenode
                                  if-undefined)))
    (cond ((string-equal val "true")
           t)
          ((string-equal val "false") nil)
          (t (error "Not a boolean value, ~A for attribute ~A."
                    val attrib)))))


;;; -*- Lisp -*-
;;; $Id$

(defpackage #:xmls-system (:use #:cl #:asdf))
(in-package :xmls-system)

(defclass xmls-source-file (cl-source-file)
  ()
  (:documentation "Component class to quash some ACL warnings.")
  )

#+allegro
(defmethod perform :around ((op compile-op) (file xmls-source-file))
  "Quash ACL warning about nested reader macros."
  (let ((excl:*warn-on-nested-reader-conditionals* nil))
    (call-next-method)))

(defsystem :xmls
    :version "1.5.1"
    :depends-on
    #+xmls-debug (:norvig)
    #-xmls-debug ()
    :components ((:file "xmls"
                        #+asdf-unicode #+asdf-unicode
                        :encoding :utf-8)
                 (:file "xmlrep-helpers"
                        ;; package is defined in XMLS. [2009/02/24:rpg]
                        :depends-on ("xmls"))))

;;; These are additional standalone tests
;;; As far as I can tell, this system is a no-op, because there is no PERFORM method
;;; for TEST-OP here, nor does test-op on XMLS invoke this system. [2012/07/21:rpg]
(defsystem :xmls-test
  :version "1.5.1"
  :depends-on (xmls nst)
  :components ((:file "nst-tests")))

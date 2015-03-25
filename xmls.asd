;;; -*- Lisp -*-
;;; $Id$

(defpackage #:xmls-system (:use #:cl #:asdf))
(in-package :xmls-system)

(defclass xmls-source-file (cl-source-file)
  ()
  (:documentation "Component class to quash some ACL warnings.")
  )

;#+allegro
;(defmethod perform :around ((op compile-op) (file xmls-source-file))
;  "Quash ACL warning about nested reader macros."
;  (let ((excl:*warn-on-nested-reader-conditionals* nil))
;    (call-next-method)))
;
(defsystem :xmls
    :version "1.7.1"
    :in-order-to ((test-op (test-op "xmls/test")))
    :components ((:file "xmls"
                        #+asdf-unicode :encoding #+asdf-unicode :utf-8)
                 (:file "xmlrep-helpers"
                        ;; package is defined in XMLS. [2009/02/24:rpg]
                        :depends-on ("xmls"))))

(defsystem :xmls/test
  :perform (test-op (op c)
              (declare (ignorable op c))
              (uiop:symbol-call :xmls :test t))
  :depends-on (xmls))







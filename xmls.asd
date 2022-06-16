;;; -*- Lisp -*-
;;; $Id$

(defpackage #:xmls-system (:use #:cl #:asdf))
(in-package :xmls-system)

(defclass xmls-source-file (cl-source-file)
  ()
  (:documentation "Component class to quash some ACL warnings.")
  )

(pushnew :xmls-nodes-are-structs *features*)

;#+allegro
;(defmethod perform :around ((op compile-op) (file xmls-source-file))
;  "Quash ACL warning about nested reader macros."
;  (let ((excl:*warn-on-nested-reader-conditionals* nil))
;    (call-next-method)))
;
(defsystem :xmls
    :version (:read-file-form "version.lisp-expr")
    :license "BSD"
    :maintainer "Robert P. Goldman <rpgoldman@sift.net>"
    :in-order-to ((test-op (test-op "xmls/test") (test-op "xmls/unit-test")))
    :components ((:file "xmls"
                        #+asdf-unicode :encoding #+asdf-unicode :utf-8)
                 (:file "xmlrep-helpers"
                        ;; package is defined in XMLS. [2009/02/24:rpg]
                        :depends-on ("xmls"))))

(defsystem :xmls/test
  :perform (test-op (op c)
              #-(or sbcl cmu allegro abcl ccl clisp ecl)
              (error "Test OP for XMLS not supported on this lisp implementation.")
              (declare (ignorable op c))
              (unless 
                (uiop:symbol-call :xmls :test :interactive t)
                (error "Failed XMLS test.")))
  :depends-on (xmls))

(defsystem xmls/unit-test
    :depends-on (xmls fiveam)
  :perform (test-op (op c)
              (declare (ignorable op c))
              (uiop:symbol-call :fiveam :run! (uiop:find-symbol* '#:xmls-test :xmls-test)))
  :components ((:file "fiveam-tests")))

(defsystem :xmls/octets
  :components ((:file "octets-xml"))
  ;; detecting the encoding doesn't work properly with flexi-streams 1.0.14
  :depends-on ("xmls" (:version  "flexi-streams" "1.0.15") "cl-ppcre")
  :version (:read-file-form "version.lisp-expr")
  :perform (test-op (op c)
              (declare (ignorable op c))
              (unless
               (uiop:symbol-call :xmls/octets :test)
                (error "Test failures in XMLS/octets."))))

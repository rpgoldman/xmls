;;; -*- Lisp -*-
;;; $Id$

(defpackage #:xmls-system (:use #:cl #:asdf))
(in-package :xmls-system)

(defsystem :xmls
    :version "1.5"
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
  :version "1.5"
  :depends-on (xmls nst)
  :components ((:file "nst-tests")))

;;; -*- Lisp -*-
;;; $Id$

(defpackage #:xmls-system (:use #:cl #:asdf))
(in-package :xmls-system)

(defsystem :xmls
    :version "1.3.1"
    :components ((:file "xmls")
                 (:file "xmlrep-helpers"
                        ;; package is defined in XMLS. [2009/02/24:rpg]
                        :depends-on ("xmls"))))


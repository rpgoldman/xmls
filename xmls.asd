;;; -*- Lisp -*-
;;; $Id$

(defpackage #:xmls-system (:use #:cl #:asdf))
(in-package :xmls-system)

(defsystem :xmls
    :version "1.2"
    :components ((:file "xmls")
		 (:file "xmlrep-helpers")))


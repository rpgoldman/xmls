(defpackage xmls-nst
  (:nicknames xmls-test)
  (:use :common-lisp :nst :xmls)
  )

(in-package :xmls-nst)

(def-test-group xmls-test ()
  (def-test check-cdata-backtrack (:equalp (list "name" nil "x]"))
    (parse "<name><![CDATA[x]]]></name>")))
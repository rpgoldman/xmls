(in-package :common-lisp-user)
(defpackage xmls-test
  (:use :common-lisp :fiveam :xmls)
  )

(in-package :xmls-test)

(def-suite xmls-test)

(in-suite xmls-test)

(test check-cdata-backtrack
  (is (equalp (make-node :name "name" :children (list "x]"))
              (parse "<name><![CDATA[x]]]></name>"))))

(test bigger-check-cdata-backtrack
  (is (equalp (make-node :name "description"
                         :children
                         (list 
 "Link to Catalog In this sequel to 2010's surprise hit, Greg Heffley, the kid who made \"wimpy\" cool is back in an all-new family comedy based on the best-selling follow-up novel by Jeff Kinney. (Kinney's Wimpy Kid\" series has thus far sold 42 million books.) As he begins seventh grade, Greg and his older brother [...]"))
              (parse "<description><![CDATA[Link to Catalog In this sequel to 2010's surprise hit, Greg Heffley, the kid who made \"wimpy\" cool is back in an all-new family comedy based on the best-selling follow-up novel by Jeff Kinney. (Kinney's Wimpy Kid\" series has thus far sold 42 million books.) As he begins seventh grade, Greg and his older brother [...]]]></description>"))))

(test simple-nodelist-translator
  (is (equalp (nodelist->node (list "description" nil
 "Link to Catalog In this sequel to 2010's surprise hit, Greg Heffley, the kid who made \"wimpy\" cool is back in an all-new family comedy based on the best-selling follow-up novel by Jeff Kinney. (Kinney's Wimpy Kid\" series has thus far sold 42 million books.) As he begins seventh grade, Greg and his older brother [...]"))
              (parse "<description><![CDATA[Link to Catalog In this sequel to 2010's surprise hit, Greg Heffley, the kid who made \"wimpy\" cool is back in an all-new family comedy based on the best-selling follow-up novel by Jeff Kinney. (Kinney's Wimpy Kid\" series has thus far sold 42 million books.) As he begins seventh grade, Greg and his older brother [...]]]></description>"))))

(def-fixture parsed-greeting ()
  (let ((node
          (with-open-file (str (asdf:system-relative-pathname "xmls" "tests/beep/greeting1.xml")
                               :direction :input)
            (parse str))))
    (&body)))

(test check-accessors
  (with-fixture parsed-greeting ()
    (is (equal "greeting" (node-name node)))
    (is (= 3 (length (node-children node))))
    (is (null (node-attrs node)))
    (is (equal (list "profile" "profile" "profile")
               (mapcar #'node-name (node-children node))))))

(test check-xmlrep-accessors
  (with-fixture parsed-greeting ()
    (is (equal "greeting" (xmlrep-tag node)))
    (is (= 3 (length (xmlrep-children node))))
    (is (null (xmlrep-attribs node)))
    (is (equal (list "profile" "profile" "profile")
               (mapcar #'xmlrep-tag (xmlrep-children node))))))


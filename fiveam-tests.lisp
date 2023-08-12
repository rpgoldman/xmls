;; (declaim (optimize (speed 0) (space 0) (debug 3) (safety 3) (compilation-speed 0)))

(in-package :common-lisp-user)
(defpackage xmls-test
  (:use :common-lisp :fiveam :xmls)
  )

(in-package :xmls-test)

(def-suite xmls-test)

(in-suite xmls-test)

(test parse-empty-document
  (null (parse "")))

(test parse-whitespace-document
  (null (parse "    ")))

(test parse-multiple-top-level-document
  (with-input-from-string (s "<a>1</a><b>2</b><c>3</c><c>4</c><c>5</c><c>6</c>")
    (is (equalp (make-node :name "a" :children '("1")) (parse s)))
    (is (equalp (make-node :name "b" :children '("2")) (parse s)))
    (is (equalp (make-node :name "c" :children '("3")) (parse s)))
    (is (equalp (make-node :name "c" :children '("4")) (parse s)))
    (is (equalp (make-node :name "c" :children '("5")) (parse s)))
    (is (equalp (make-node :name "c" :children '("6")) (parse s)))
    (null (parse s))))

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

(test attribute-with-prefixed-name
      (is (string= "style"
		   (getf (xmls::find-attrib "name"
					    (xmls:parse "<?xml version=\"1.0\"?> <text:list-style style:name=\"L1\"></text:list-style>"))
			 :attr-ns))))

(def-fixture article-parsed-as-list ()
  (let ((parse-tree
          (with-open-file (str (asdf:system-relative-pathname "xmls" "tests/nxml/genetics-article.xml")
                               :direction :input)
            (xmls:parse-to-list str))))
    (&body)))

(test check-extract-path-from-list-nodes
  (with-fixture article-parsed-as-list ()
    ;; retrieve first of items matching the path
     (let ((result (xmls:extract-path '("OAI-PMH" "GetRecord" "record" "metadata" "article"
                                       "back" "ref-list" "ref")
                                     parse-tree)))
      (is (= 4 (length result)))
      (is (equalp (nth 0 result)
                  '("ref" . "http://dtd.nlm.nih.gov/2.0/xsd/archivearticle")))
      (is (equalp (nth 1 result)
                  '(("id" "gkt903-B1")))))

    ;; retrieve tag attributes of first item matching the path
    (let ((result (xmls:extract-path '("OAI-PMH" "GetRecord" "record" "metadata" "article"
                                       "back" "ref-list" "ref" . *)
                                     parse-tree)))
      (is (equalp result
                  '(("id" "gkt903-B1")))))

    ;; retrieve all items enclosed by element matching the path
    (let ((result (xmls:extract-path '("OAI-PMH" "GetRecord" "record" "metadata" "article"
                                       "back" "ref-list" *)
                                     parse-tree)))
      (is (= 41 (length result)))
      (is (equalp (nth 0 result)
                  '(("title" . "http://dtd.nlm.nih.gov/2.0/xsd/archivearticle") nil "REFERENCES")))
      (is (equalp (nth 1 (nth 1 result))
                  '(("id" "gkt903-B1"))))
      (is (equalp (nth 1 (nth 15 result))
                  '(("id" "gkt903-B15")))))

    ;; select specific item among several with same tag based on tag attributes
    ;; here selecting on "ref" in the path...
    (let ((result (xmls:extract-path '("OAI-PMH" "GetRecord" "record" "metadata" "article"
                                       "back" "ref-list" ("ref" ("id" "gkt903-B15")) "element-citation"
                                       "article-title")
                                     parse-tree)))
      (is (equalp result
                  '(("article-title" . "http://dtd.nlm.nih.gov/2.0/xsd/archivearticle") NIL
                    "HNS, a nuclearcytoplasmic shuttling sequence in HuR"))))))

(def-fixture article-parsed-as-struct ()
  (let ((parse-tree
          (with-open-file (str (asdf:system-relative-pathname "xmls" "tests/nxml/genetics-article.xml")
                               :direction :input)
            (xmls:parse str))))
    (&body)))

(test check-extract-path-from-struct-nodes
  (with-fixture article-parsed-as-struct ()
    ;; retrieve first of items matching the path
    (let ((result (xmls:extract-path '("OAI-PMH" "GetRecord" "record" "metadata" "article"
                                       "back" "ref-list" "ref")
                                     parse-tree)))
      (is (string= (node-name result) "ref"))
      (is (string= (node-ns result) "http://dtd.nlm.nih.gov/2.0/xsd/archivearticle"))
      (is (equalp (node-attrs result) '(("id" "gkt903-B1")))))

    ;; retrieve tag attributes of first item matching the path
    (let ((result (xmls:extract-path '("OAI-PMH" "GetRecord" "record" "metadata" "article"
                                       "back" "ref-list" "ref" . *)
                                     parse-tree)))
      (is (equalp result '(("id" "gkt903-B1")))))

    ;; retrieve all items enclosed by element matching the path
    (let ((result (xmls:extract-path '("OAI-PMH" "GetRecord" "record" "metadata" "article"
                                       "back" "ref-list" *)
                                     parse-tree)))
      (is (= 41 (length result)))
      (is (equalp (nth 0 result)
                  (make-node :name "title"
                             :ns "http://dtd.nlm.nih.gov/2.0/xsd/archivearticle"
                             :attrs nil
                             :children '("REFERENCES"))))
      (is (equalp (node-attrs (nth 1 result))
                  '(("id" "gkt903-B1"))))
      (is (equalp (node-attrs (nth 15 result))
                  '(("id" "gkt903-B15")))))

    ;; select specific item among several with same tag based on tag attributes
    ;; here selecting on "ref" in the path...
    (let ((result (xmls:extract-path '("OAI-PMH" "GetRecord" "record" "metadata" "article"
                                       "back" "ref-list" ("ref" ("id" "gkt903-B15")) "element-citation"
                                       "article-title")
                                     parse-tree)))
      (is (equalp result
                  (make-node :name "article-title"
                             :ns "http://dtd.nlm.nih.gov/2.0/xsd/archivearticle"
                             :attrs NIL
                             :children '("HNS, a nuclearcytoplasmic shuttling sequence in HuR")))))))

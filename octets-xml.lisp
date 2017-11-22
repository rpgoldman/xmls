;;;; Octet stream support for XMLS with automatic encoding detection.
;;;;
;;;; FIXME: Could/should use parser rules native to XMLS to parse the XML
;;;; declaration.

(defpackage xmls/octets
  (:use :cl
        :flexi-streams
        :cl-ppcre)
  (:export :make-xml-stream))

(in-package :xmls/octets)

;;; XML defines heuristics to determine the encoding of a document represented
;;; by an octet stream: https://www.w3.org/TR/xml/#charencoding

(defparameter *pi-start* (string-to-octets "<?"  :external-format :ascii))
(defparameter *pi-end*   (string-to-octets "?>"  :external-format :ascii))
(defparameter *xml*      (string-to-octets "xml" :external-format :ascii))

(defparameter *max-decl-size* 2000 ; extra gracious margin
  "XML declarations exceeding *MAX-DECL-SIZE* bytes are ignored.")
(defvar *encoding-scanner*)

(eval-when (:load-toplevel :execute)
  (let ((s "[\\s\\r]*")                 ; (#x20 | #x9 | #xD | #xA)+?
        (enc-name "[A-Za-z0-9\\._-]+")) ; [A-Za-z] ([A-Za-z0-9._] | '-')*
    (setf *encoding-scanner*
      ;; S 'encoding' Eq ('"' EncName '"' | "'" EncName "'")
      (create-scanner (format nil "encoding~a=~a(\"(~a)\"|'(~a)')"
                              s s enc-name enc-name)))))

(defun xml-encoding-decl (string)
  (register-groups-bind (() a b) (*encoding-scanner* string)
    (external-format-name (make-external-format (or a b)))))

(defun xml-encoding (in)
  (let ((buffer (make-array *max-decl-size* :element-type '(unsigned-byte 8)))
        (pos 0))
    (labels ((fill-buffer (n)
               (setf pos (read-sequence buffer in :start pos :end (+ pos n))))
             (buffer-to-ascii ()
               (octets-to-string buffer :end pos :external-format :ascii))
             (return-encoding (&optional (encoding :utf-8)
                                         (read (buffer-to-ascii)))
               (return-from xml-encoding (values encoding read))))
      (fill-buffer 2)
      (when (search #(#xFE #xFF) buffer :end2 pos)
        (return-encoding :utf-16be ""))
      (when (search #(#xFF #xFE) buffer :end2 pos)
        (return-encoding :utf-16le ""))
      (unless (search *pi-start* buffer :end2 pos)
        (return-encoding))
      (fill-buffer 3)
      (unless (search *xml* buffer :start2 2 :end2 pos)
        (return-encoding))
      (loop when (= pos (fill-buffer 1)) do (return-encoding)
         until (search *pi-end* buffer :start2 (- pos 2) :end2 pos))
      (let ((read (buffer-to-ascii)))
        (return-encoding (xml-encoding-decl read) read)))))

(defun make-xml-stream (octet-stream)
  "Determine character encoding and return an XML stream for OCTET-STREAM."
  (multiple-value-bind (encoding read) (xml-encoding octet-stream)
    (make-concatenated-stream (make-string-input-stream read)
                              (make-flexi-stream octet-stream
                                                 :external-format encoding
                                                 :element-type 'character))))


;;; Test cases

(defun test ()
  (let* ((flux-test (asdf:system-relative-pathname
                     :xmls/octets "octets-tests/flux/flux-test.sexp"))
         (reference (with-open-file (in flux-test)
                      (read in))))
    (loop with success-p = t
          for test-case in (mapcar #'(lambda (x)
                                       (asdf:system-relative-pathname "xmls" (format nil "octets-tests/flux/~a" x)))
                                   '("flux-test-iso-8859-1.xml"
                                     "flux-test-utf-16le.xml"
                                     "flux-test-utf-16be.xml"
                                     "flux-test-utf-8.xml"))
          for parsed = (with-open-file (octets (asdf:system-relative-pathname
                                                :xmls/octets test-case)
                                               :element-type '(unsigned-byte 8))
                         (xmls:parse (make-xml-stream octets)))
          as parsed-list = (xmls:node->nodelist parsed)
          do (when (not (equal parsed-list reference))
               (setf success-p nil)
               (format *error-output* "~&Test failure: ~a~%Output was: ~s~%~%" test-case parsed))
          finally (return success-p))))

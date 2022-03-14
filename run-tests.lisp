(defpackage xmls-test-runner
  (:use :common-lisp))

(in-package :xmls-test-runner)

(defun featurep (x)
  (member x *features* :test 'eq))

(require :asdf)
(format t "ASDF version is ~a~%" (asdf:asdf-version))
(defparameter *quicklisp-p* (or (featurep :quicklisp)
                                (not (zerop (parse-integer (uiop:getenv "QUICKLISP"))))) )
;; Roswell puts its quicklip set up in a different place.
(when (and  *quicklisp-p* (not (featurep :ros.init)))
  (load (merge-pathnames "quicklisp/setup.lisp" 
                           (user-homedir-pathname))))
(defmacro quit-on-error (&body body)
  (let ((code 1))
   (when (numberp (first body))
     (setf code (pop body)))
    `(call-quitting-on-error (lambda () ,@body) ,code)))

(defun call-quitting-on-error (thunk &optional (code 1))
  "Unless the environment variable DEBUG_ASDF_TEST
is bound, write a message and exit on an error.  If
*asdf-test-debug* is true, enter the debugger."
  (flet ((quit (c desc)
           (uiop:safe-format! *error-output* "~&Encountered ~a during test.~%~a~%" desc c)
           (cond
            ;; decline to handle the error.
            ((ignore-errors (funcall (find-symbol "GETENV" :asdf) "DEBUG_ASDF_TEST"))
             (format t "~&Interactive mode (DEBUG_ASDF_TEST) -- Invoke debugger.~%")
             (invoke-debugger c))
            (t
             (finish-output *standard-output*)
             (finish-output *trace-output*)
             (uiop:safe-format! *error-output* "~&ABORTING:~% ~S~%" c)
             (uiop:print-condition-backtrace c)
             (uiop:safe-format! *error-output* "~&ABORTING:~% ~S~%" c)
             (uiop:safe-format! *error-output* "~&Script failed~%")
             (finish-output *error-output*)
             (uiop:quit code t)))))
    (handler-bind
        ((error (lambda (c)
                  (quit c  "ERROR")))
         (storage-condition
          (lambda (c) (quit c "STORAGE-CONDITION")))
         (serious-condition (lambda (c)
                              (quit c "Other SERIOUS-CONDIITON"))))
      (funcall thunk)
      (format t "~&Script succeeded~%")
      t)))


;; for this to work, we must ensure that ASDF gets an OK configuration
;; on startup.
(setf asdf:*compile-file-failure-behaviour* :error)
(quit-on-error
 (macrolet ((load-system (s)
                         (if *quicklisp-p* `(uiop:symbol-call '#:ql '#:quickload ,s)`(asdf:load-system ,s))))
   (load-system :flexi-streams)
   (load-system :fiveam)
   (load-system "cl-ppcre")))               ; need to do this here because it doesn't build without warnings.
(setf asdf:*compile-file-warnings-behaviour* :error)
(defvar *build-warning* nil)
(defvar *build-error* nil)
(catch 'build-fail
 (handler-bind ((warning #'(lambda (x)
                             ;; this is necessary because on SBCL
                             ;; there's an EXTERNAL handler for some
                             ;; uninteresting warnings.
                             (signal x)
                             (push x *build-warning*)
                             (throw 'build-fail :fail)))
                (error #'(lambda (x)
                           (push x *build-error*)
                           (throw 'build-fail :warn))))
   (asdf:load-system "xmls" :force t)))
(cond (*build-error*
       (uiop:die 1 "XMLS build failed with error(s):~%~{~a~%~}"
               *build-error*))
      (*build-warning*
       (uiop:die 1 "XMLS build failed with warning(s):~%~{~a~%~}"
               *build-warning*)))

(catch 'build-fail
 (handler-bind ((warning #'(lambda (x)
                             ;; this is necessary because on SBCL
                             ;; there's an EXTERNAL handler for some
                             ;; uninteresting warnings.
                             (signal x)
                             (push x *build-warning*)
                             (throw 'build-fail :fail)))
                (error #'(lambda (x)
                           (push x *build-error*)
                           (throw 'build-fail :warn))))
   (asdf:load-system "xmls/octets" :force t)))
(cond (*build-error*
       (uiop:die 2 "XMLS/OCTETS build failed with error(s):~%~{~a~%~}"
               *build-error*))
      (*build-warning*
       (uiop:die 2 "XMLS/OCTETS build failed with warning(s):~%~{~a~%~}"
               *build-warning*)))


(quit-on-error
  3
 (format t "~&;;; Testing XMLS.~%")
 (asdf:test-system "xmls"))

(quit-on-error
  4
 (format t "~&;;; Testing XMLS/OCTETS.~%")
 (asdf:test-system "xmls/octets"))

(uiop:quit 0)

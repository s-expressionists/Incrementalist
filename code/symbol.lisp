(cl:in-package #:incrementalist)

(defclass package-client () ())

(defvar *package-client* (make-instance 'package-client))

#+unused? (defclass symbol ()
  ((%name    :initarg :name
             :type    string
             :reader  name)
   (%package :initarg :package
             ; :type    (or null ?)
             :reader  package))
  ;; TODO: missing initargs
  )

(defun make-package-system-client ()
  (let ((client (make-instance 'parcl.test::mock-client)))
    (let* ((pkg:*client*    client)
           (keyword         (pkg:make-package "KEYWORD"))
           ;; TODO: give size hint for CL package
           (cl              (pkg:make-package "COMMON-LISP" :nicknames '("CL")))
           (cl-user         (pkg:make-package "COMMON-LISP-USER"
                                              :nicknames '("CL-USER")
                                              :use       '("CL")))
           (eclector.reader (pkg:make-package "ECLECTOR.READER")))
      (declare (ignore keyword cl-user))
      (do-external-symbols (symbol "CL")
        (pkg:export (pkg:intern (symbol-name symbol) cl) cl))
      (dolist (name '("QUASIQUOTE" "UNQUOTE" "UNQUOTE-SPLICING"))
        (pkg:intern name eclector.reader)))
    client))

(setf pkg:*client* (make-package-system-client))

(cl:in-package #:incrementalist)

(defclass token () ())

(#+sbcl sb-ext:defglobal #-sbcl defvar **keyword-package-name**
  "KEYWORD")

(#+sbcl sb-ext:defglobal #-sbcl defvar **common-lisp-package-name**
  "COMMON-LISP")

(defun intern-package-name (package-name)
  (cond ((string= package-name **common-lisp-package-name**)
         **common-lisp-package-name**)
        ((string= package-name **keyword-package-name**)
         **keyword-package-name**)
        (t
         package-name)))

(#+sbcl sb-ext:defglobal #-sbcl defvar **nil-symbol-name**
  "NIL")

(defun intern-symbol-name (symbol-name)
  (cond ((string= symbol-name **nil-symbol-name**)
         **nil-symbol-name**)
        (t
         symbol-name)))

(defclass symbol-token (token)
  ((%package-marker-1 :initarg  :package-marker-1
                      :type     (or null integer)
                      :reader   package-marker-1
                      :initform nil)
   (%package-marker-2 :initarg  :package-marker-2
                      :type     (or null integer)
                      :reader   package-marker-2
                      :initform nil)
   (%package-name     :initarg  :package-name
                      :type     (or null string)
                      :reader   package-name
                      :initform nil)
   (%name             :initarg  :name
                      :type     string
                      :reader   name)))

(defun maybe-truncate-string (thing &key (limit 32))
  (cond ((not (stringp thing))
         thing)
        ((<= (length thing) limit)
         thing)
        (t
         (concatenate 'string (subseq thing 0 (1- limit)) "…"))))

(defmethod print-object ((object symbol-token) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "[~A]~:[~;:~]~:[~;:~]~A"
            (maybe-truncate-string (package-name object))
            (package-marker-1 object)
            (package-marker-2 object)
            (maybe-truncate-string (name object)))))

(defclass non-existing-package-symbol-token (symbol-token)
  ())

(defclass non-existing-symbol-token (symbol-token)
  ())

(defclass existing-symbol-token (symbol-token)
  ())

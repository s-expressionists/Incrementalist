(cl:in-package #:incrementalist)

;; TODO: the classes and methods in this file should eventually go back into parcl

;;; `mock-package' class ; TODO: chose a different name

(defclass mock-package (parcl.low:package)
  (;; Client
   (%client               :initarg  :client ; TODO: check whether this is ever used
                          :reader   client)
   ;; Names
   (%name                 :initarg  :name
                          :accessor %name)
   (%nicknames            :accessor %nicknames
                          :type     list ; of string
                          :initform '())
   ;; Nicknames
   (%local-nicknames      :accessor %local-nicknames
                          :type     list ; of (NICKNAME PACKAGE)
                          :initform '())
   (%locally-nicknamed-by :accessor %locally-nicknamed-by
                          :type     list ; of `package'
                          :initform '())
   ;; Use relations
   (%uses                 :accessor %uses
                          :type     list ; of `package'
                          :initform '())
   (%used-by              :accessor %used-by
                          :type     list ; of `package'
                          :initform '())

   ;; Symbols
   (%entries              :reader   %entries
                          :type     hash-table
                          :initform (make-hash-table :test #'equal))
   ;; Documentation
   (%documentation        :accessor %documentation
                          :type     (or null string)
                          :initform nil)))

(defmethod print-object ((object mock-package) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (%name object))))

;;;; `mock-symbol' class

(defclass mock-symbol () ; TODO: choose some other name
  ((%name    :initarg  :name
             :type     string
             :reader   %name)
   (%package :initarg  :package
             :accessor %package
             :initform nil)))

(defmethod print-object ((object mock-symbol) stream)
  (let* ((name    (%name object))
         (package (%package object)))
    (cond ((typep package '(or null mock-package))
           (let ((package-name  (if (null package)
                                    nil
                                    (%name package)))
                 (export-status (if (null package)
                                    :uninterned
                                    (cadr (gethash name (%entries package))))))
             (format stream "~@[|~A|~]~A|~A|"
                     package-name (ecase export-status
                                    (:uninterned "#:")
                                    (:internal   "::")
                                    (:external   ":"))
                     name)))
          (t
           (print-unreadable-object (object stream :type t :identity t)
             (format stream "~S~@[ in ~A~]" name package))))))

;;;; `package-system-client' class

(defclass package-system-client (parcl.middle:local-nicknames-mixin)
  ((%packages :type     hash-table
              :reader   %packages
              :initform (make-hash-table :test #'equal))))

;;; Symbol functions

(defmethod parcl.low:symbolp ((client package-system-client)
                              (object mock-symbol))
  t)

(defmethod parcl.low:symbol-name ((client package-system-client)
                                  (symbol mock-symbol))
  (%name symbol))

(defmethod parcl.low:symbol-package ((client package-system-client)
                                     (symbol mock-symbol))
  (%package symbol))

(defmethod (setf parcl.low:symbol-package) ((new-value t)
                                            (client    package-system-client)
                                            (symbol    mock-symbol))
  (setf (%package symbol) new-value))

(defmethod parcl.low:make-symbol ((client  package-system-client)
                                  (name    string)
                                  (package t))
  (make-instance 'mock-symbol :name name :package package))

;;; Package functions

(defmethod parcl.low:packagep ((client package-system-client)
                               (object mock-package))
  t)

(macrolet ((define-accessor (protocol-name implementation-name)
             `(progn
                (defmethod ,protocol-name ((client  package-system-client)
                                           (package mock-package))
                  (,implementation-name package))

                (defmethod (setf ,protocol-name) ((new-value t)
                                                  (client    package-system-client)
                                                  (package   mock-package))
                  (setf (,implementation-name package) new-value)))))

  (define-accessor parcl.low:name                 %name)
  (define-accessor parcl.low:nicknames            %nicknames)
  (define-accessor parcl.low:use-list             %uses)
  (define-accessor parcl.low:used-by-list         %used-by)
  (define-accessor parcl.low:local-nicknames      %local-nicknames)
  (define-accessor parcl.low:locally-nicknamed-by %locally-nicknamed-by)
  (define-accessor parcl.low:documentation        %documentation))

(defmethod parcl.low:make-package-object ((client package-system-client)
                                          (name   string))
  (make-instance 'mock-package :client client :name name))

;;; Package-symbol relation functions

(defmethod parcl.low:map-symbol-entries ((client   package-system-client)
                                         (function t)
                                         (package  mock-package)
                                         &optional status)
  (declare (ignore status))
  (maphash (lambda (name entry)
             (declare (ignore name))
             (destructuring-bind (symbol . (export-status . shadow-status))
                 entry
               (funcall function symbol export-status shadow-status)))
           (%entries package)))

(defmethod parcl.low:symbol-entry ((client  package-system-client)
                                   (name    string)
                                   (package mock-package))
  (let ((entry (gethash name (%entries package))))
    (if (null entry)
        (values nil nil nil)
        (values (car entry) (cadr entry) (cddr entry)))))

(defmethod parcl.low:set-symbol-entry ((new-symbol        t)
                                       (new-export-status t)
                                       (new-shadow-status t)
                                       (client            package-system-client)
                                       (name              string)
                                       (package           mock-package))
  ;; TODO: remove later
  (when (null new-export-status) (assert (null new-shadow-status)))
  (let ((entries (%entries package)))
    (if (null new-export-status)
        (remhash name entries)
        (setf (gethash name entries)
              (cons new-symbol (cons new-export-status new-shadow-status))))))

;;; Environment functions

(defmethod parcl.low:packages ((client package-system-client))
  (loop :for name :being the :hash-keys :of (%packages client) :using (:hash-value package)
        :when (equal name (parcl.low:name client package))
          :collect package))

(defmethod parcl.low:find-package ((client             package-system-client)
                                   (package-designator string))
  (gethash package-designator (%packages client)))

(defmethod (setf parcl.low:find-package)
    ((new-value t) (client package-system-client) (name string))
  (setf (gethash name (%packages client)) new-value))

(defmethod (setf parcl.low:find-package)
    ((new-value null) (client package-system-client) (name string))
  (remhash name (%packages client)))

(defvar *package-client* (make-instance 'package-system-client))

;;;

#+unused? (defclass symbol ()
            ((%name    :initarg :name
                       :type    string
                       :reader  name)
             (%package :initarg :package
                                        ; :type    (or null ?)
                       :reader  package))
            ;; TODO: missing initargs
            )

(defun make-package-minimal-system-client ()
  (let ((client (make-instance 'package-system-client)))
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

(defun make-package-system-client ()
  (let* ((client       (make-instance 'package-system-client))
         (pkg:*client* client))
    (labels ((copy-external-symbols (source-package target-package)
               (let ((source-package (find-package source-package))
                     (shadowing      (mapcar #'symbol-name (package-shadowing-symbols
                                                            source-package))))
                 (pkg:shadow shadowing target-package)
                 (do-external-symbols (symbol source-package)
                   (when (eq (symbol-package symbol) source-package)
                     (let ((new-symbol (pkg:intern (symbol-name symbol) target-package)))
                       (pkg:export new-symbol target-package))))))
             (copy-package (source-package)
               (let* ((package-name (package-name source-package))
                      (package      (pkg:find-package package-name)))
                 (format *trace-output* "Copying ~32A -> ~32A~%" source-package package)
                 (if (not (null package))
                     package
                     (let* ((nicknames       (package-nicknames source-package))
                            (used            (package-use-list source-package))
                            (local-nicknames (sb-ext:package-local-nicknames source-package))
                            (package         (pkg:make-package
                                              package-name :nicknames nicknames)))
                       (copy-external-symbols package-name package)
                       (parcl:use-package (mapcar #'copy-package used) package)
                       (loop :for (nickname . nicknamed-package) :in local-nicknames
                             :do (pkg:add-package-local-nickname
                                  nickname (copy-package nicknamed-package) package))
                       package)))))
      (mapc #'copy-package (list-all-packages)))
    client))

(setf pkg:*client* (make-package-system-client))

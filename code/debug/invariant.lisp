(cl:in-package #:incrementalist.debug)

;;; The `define-invariant' macro expands, for a given function, to one
;;; or more invariant checking functions and, if supported by the
;;; implementation, one or more `cl:trace' calls which ensure that the
;;; checking function(s) is/are called before and/or after calls to
;;; the function under consideration.

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl
  (defun emit-trace (function-name &key ((:before before-spec))
                                        ((:after  after-spec)))
    (labels  ((emit-argument (position)
                `(handler-case
                     (sb-debug:arg ,position)
                   (sb-kernel:index-too-large-error () nil)))
              (emit-print (option spec)
                (destructuring-bind (function argument-positions) spec
                  `(,option (progn
                              (,function
                               ,@(mapcar #'emit-argument argument-positions))
                              (values))))))
      `(trace ,function-name :report nil
              ,@(when before-spec
                  (emit-print :print before-spec))
              ,@(when after-spec
                  (emit-print :print-after after-spec)))))

  #-sbcl
  (defun emit-trace (function-name &key before after)
    (declare (ignore before after))
    (warn "~@<Unable to attach invariant checks to function ~A in implemntation ~
           ~A ~A.~@:>"
          function-name
          (lisp-implementation-type) (lisp-implementation-version))
    nil))

(defmacro define-invariant (function-name &key before after)
  (let ((traces '()))
    (flet ((emit-checking-function (when spec)
             (destructuring-bind (parameters &body body) spec
               (let ((name      (alexandria:symbolicate when '#:- function-name))
                     (names     (mapcar #'first parameters))
                     (positions (mapcar #'second parameters)))
                 (alexandria:appendf traces `(,when (,name ,positions)))
                 `(defun ,name ,names ,@body)))))
      `(progn
         ,@(when before
             `(,(emit-checking-function :before before)))
         ,@(when after
             `(,(emit-checking-function :after after)))
         ,(apply 'emit-trace function-name traces)))))

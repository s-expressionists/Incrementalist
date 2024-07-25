(cl:in-package #:incrementalist.debug)

(defvar *enabled-categories* '())

(defmacro log (category format-control &rest format-arguments)
  (alexandria:with-unique-names (enabled)
    `(let ((,enabled *enabled-categories*))
       (when (or (eq ,enabled t) (member ',category ,enabled))
         (let ((*print-pretty* t))
           (format *trace-output* ,format-control ,@format-arguments))))))

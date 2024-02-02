(cl:in-package #:incrementalist)

(defun update-cache (analyzer)
  (scavenge (cache analyzer))
  (read-forms analyzer))

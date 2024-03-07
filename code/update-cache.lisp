(cl:in-package #:incrementalist)

(defmethod update ((analyzer analyzer))
  (scavenge (cache analyzer))
  (read-forms analyzer))

(cl:in-package #:incrementalist)

(defmethod update ((analyzer analyzer))
  (let* ((buffer      (buffer analyzer))
         (buffer-time (b:current-time buffer))
         (cache       (cache analyzer))
         (cache-time  (time-stamp cache)))
    ;; Skip `scavenge' and `read-forms' if the buffer content has not
    ;; changed since the most recent update.
    (when (or (null cache-time) (> buffer-time cache-time))
      (scavenge cache)
      (read-forms analyzer))))

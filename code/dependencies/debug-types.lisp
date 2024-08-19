(cl:in-package #:incrementalist.dependencies)

(defun %every-cell (object)
  (and (listp object)
       (every (alexandria:of-type 'cell) object)))

(defun %distinct-aspects (object)
  (and (listp object)
       (let ((seen-aspects '()))
         (every (lambda (cell)
                  (let ((aspect (aspect cell)))
                    (if (find aspect seen-aspects :test #'eq)
                        nil
                        (progn
                          (push aspect seen-aspects)
                          t))))
                object))))

(deftype list-of-cells ()
  `(and list (satisfies %every-cell)))

(deftype list-of-cells-for-distinct-aspects ()
  `(and list
        (satisfies %every-cell)
        (satisfies %distinct-aspects)))

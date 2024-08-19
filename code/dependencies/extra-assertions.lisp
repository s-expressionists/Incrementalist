(cl:in-package #:incrementalist.dependencies)

;;; Class `cell'

(defmethod remove-user :before ((user t) (cell cell))
  (assert (not (null (%users cell)))))

;;; Class `cell-user-mixin'

(dbg:define-invariant add-dependencies
  :before (((object 0) (defined-cells 1) #|(used-cells 2)|# (escaping-cells 3))
           (assert (= (length defined-cells)
                      (length (remove-duplicates defined-cells :test #'eq))))
           (loop :for cell :in escaping-cells
                 :do (assert (member (scope cell)
                                     '(:top-level-expression :global)))
                     (assert (not (find cell (defined object)))))))

(defmethod inheritable :before ((object cell-user-mixin))
  ;; Since this is only called for top-level objects, all escaping
  ;; cells must have global scope.
  (assert (every (lambda (cell) (eq (scope cell) :global)) (escaped object)))
  ;; Every external cell overwrites an inherited cell.
  (let ((inherited (inherited object)))
    (map-external
     (lambda (cell)
       (assert (find (aspect cell) inherited :test #'eq :key #'aspect)))
     object :external-scopes '(:global))))

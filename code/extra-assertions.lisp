(cl:in-package #:incrementalist)

;;; Model checks

(defun check-absolute-wad-with-relative-descendants (wad)
  (assert (not (relative-p wad)))
  (when (typep wad 'cst:cst)
    (assert (null (cst:source wad))))
  (labels ((check-child (child)
             (assert (relative-p child))
             (map-children #'check-child child)))
    (map-children #'check-child wad))
  wad)

(defun check-absolute-line-numbers (top-level-wad)
  ;; Make sure the wad itself is absolute, so that we need to compute
  ;; the absolute line numbers only of its children.
  (assert (not (relative-p top-level-wad)))
  (assert (eql (absolute-start-line top-level-wad)
               (start-line top-level-wad)))
  (labels ((process-children (relative-wads offset)
             (loop with base = offset
                   for wad in relative-wads
                   for absolute-start-line = (+ base (start-line wad))
                   do (assert (eql (absolute-start-line wad)
                                   absolute-start-line))
                      (process-wad wad absolute-start-line)
                      (setf base absolute-start-line)))
           (process-wad (wad wad-start-line)
             (when (typep wad 'cst:cst)
               (assert (null (cst:source wad))))
             (let ((children (children wad)))
               (assert (every #'relative-p children))
               (process-children children wad-start-line))
             (loop with base = wad-start-line
                   for error-wad in (errors wad)
                   for absolute-start-line = (+ base (start-line error-wad))
                   do (assert (relative-p error-wad))
                   do (assert (eql (absolute-start-line error-wad)
                                   absolute-start-line))
                   (setf base absolute-start-line))))
    (process-wad top-level-wad (start-line top-level-wad))))

(defun report (message &rest arguments)
  (apply #'format *trace-output* message arguments))

(defun check-wad (wad parent prefix-p)
  (unless (eq (parent wad) parent)
    (report "a: Parent of ~s is ~s rather than ~s~%"
            wad (parent wad) parent))
  (let ((children (children wad)))
    (unless (null children)
      (unless (null (left-sibling (first children)))
        (report "a: Left sibling of ~s is ~s rather than NIL~%"
                (first children) (left-sibling (first children))))
      (unless (null (right-sibling (first (last children))))
        (report "a: Right sibling of ~s is ~s rather than NIL~%"
                (first (last children))
                (right-sibling (first (last children)))))
      (loop for (left right) on children
            until (null right)
            do (unless (eq (right-sibling left) right)
                 (report "a: Right sibling of ~s is ~s rather than ~s~%"
                         left (right-sibling left) right))
               (unless (eq (left-sibling right) left)
                 (report "a: Left sibling of ~s is ~s rather than ~s~%"
                         right (left-sibling right) left))))
    (loop for child in children
          do (check-wad child wad prefix-p)))

  (when prefix-p
    (assert (stringp (items wad)))))

(defun check-wad-graph (cache)
  (let ((prefix (prefix cache))
        (suffix (suffix cache)))
    (assert (null (intersection prefix suffix)))
    (unless (null prefix)
      (unless (null (left-sibling (first (last prefix))))
        (report "b: Left sibling of last (~s) is ~s rather than NIL.~s"
                (first (last prefix)) (left-sibling (first (last prefix)))))
      (loop for (right left) on prefix
            until (null left)
            do (unless (eq (left-sibling right) left)
                 (report "b: Left sibling of ~s is ~s rather than ~s~%"
                         right (left-sibling right) left))
               (unless (eq (right-sibling left) right)
                 (report "b: Right sibling of ~s is ~s rather than ~s~%"
                         left (right-sibling left) right))))
    (unless (null suffix)
      (unless (null (right-sibling (first (last suffix))))
        (report "c: Right sibling of last (~s) is ~s rather than NIL.~%"
                (first (last suffix))
                (right-sibling (first (last suffix)))))
      (loop for (left right) on suffix
            until (null right)
            do (unless (eq (left-sibling right) left)
                 (report "c: Left sibling of ~s is ~s rather than ~s~%"
                         right (left-sibling right) left))
               (unless (eq (right-sibling left) right)
                 (report "c: Right sibling of ~s is ~s rather than ~s~%"
                         left (right-sibling left) right))))
    (if (null prefix)
        (if (null suffix)
            nil
            (unless (null (left-sibling (first suffix)))
              (report "d: Left sibling of ~s is ~s rather than NIL~%"
                      (first suffix) (left-sibling (first suffix)))))
        (if (null suffix)
            (unless (null (right-sibling (first prefix)))
              (report "d: Right sibling of ~s is ~s rather than NIL~%"
                      (first prefix) (right-sibling (first prefix))))
            (progn
              (unless (eq (right-sibling (first prefix)) (first suffix))
                (report "e: Right sibling of ~s is ~s rather than ~s~%"
                        (first prefix)
                        (right-sibling (first prefix))
                        (first suffix)))
              (unless (eq (left-sibling (first suffix)) (first prefix))
                (report "e: Left sibling of ~s is ~s rather than ~s~%"
                        (first suffix)
                        (left-sibling (first suffix))
                        (first prefix))))))
    (loop for wad in prefix do (check-wad wad nil t))
    (loop for wad in suffix do (check-wad wad nil nil))))

;;; Model invariants

(defmethod absolute-start-line :before ((wad wad))
  ;; This used to assert that the top-level wad which contains WAD is
  ;; absolute but we use the cached absolute line number more
  ;; aggressively than that now.
  (assert (not (eql :invalid (slot-value wad '%absolute-start-line)))))

(defvar *duplicate-children-allowed-p* nil)
(defmethod map-children :around ((function t) (node t))
  (let ((seen     (make-hash-table :test #'eq))
        (previous nil))
    (flet ((do-it (child)
             ;; Duplicates
             (unless *duplicate-children-allowed-p*
               (assert (not (gethash child seen))))
             (setf (gethash child seen) t)
             ;; Buffer location order
             (when previous
               ;; TODO would be nice to assert this
               ;; (assert (wad-ends-after-wad-p previous child))
               )
             (setf previous child)
             ;; Original function
             (funcall function child)))
      (call-next-method #'do-it node))))

(defmethod (setf left-sibling) :before ((new-value t) (object t))
  (assert (not (eq new-value object))))

;;; Reader

(dbg:define-invariant make-children-relative-and-set-family-relations
  :before (((wad 0))
           (assert (not (relative-p wad)))
           (map-children (lambda (child)
                           (assert (not (eq child (first (suffix (cache child))))))
                           (assert (null (parent child)))
                           (check-absolute-wad-with-relative-descendants child))
                         wad)))

(dbg:define-invariant adjust-result
  :before (((result 0) (keyword 4) (extra-children 5))
           (assert (not (typep result 'wad)))
           (when (and (eq keyword :children) (not (null extra-children)))
             (mapc (lambda (child)
                     (check-absolute-wad-with-relative-descendants child))
                   extra-children)))
  :after  (((result 0))
           (labels ((check-wad (result)
                      (assert (not (relative-p result)))
                      (assert (eq (absolute-start-line result) (start-line result)))
                      (let ((children (children result)))
                        (cond ((null children))
                              ((some #'relative-p children)
                               (check-absolute-line-numbers result)) ; asserts that children are relative
                              (t
                               (mapc #'check-wad children))))))
             (when (typep result 'wad)
               (check-wad result)))))

(defmethod eclector.parse-result:make-expression-result
    :around ((client client) (result t) (children t) (source t))
  ;; When WADs appear as children, they have to be absolute WADs while
  ;; their respective descendants have to be relative (as always). The
  ;; purpose of checking this recursively here is mainly to catch
  ;; inadvertent mutation after the creation of the WADs.
  (let ((previous nil))
    (mapc (lambda (child)
            (assert (null (parent child)))
            (check-absolute-wad-with-relative-descendants child)
            (when previous
              (assert (%position< (start-line previous) (start-column previous)
                                  (start-line child)    (start-column child))))
            (setf previous child))
          children))
  ;; The return value must be an absolute WAD with relative
  ;; descendants. The returned WAD either remains absolute and becomes
  ;; a top-level WAD or becomes the child of another WAD and is made
  ;; relative at that point.
  (check-absolute-wad-with-relative-descendants
   ;; Within the `make-expression-result' result method, the result
   ;; may temporarily have duplicate children: `map-children' is
   ;; called on a `cons-cst' to check whether the children defined by
   ;; the CST structure are already valid. However, the CST-based
   ;; children are not valid and contain repeated children for objects
   ;; produced by certain reader macros such as a reader macro
   ;; #{ 1 2 } which returns (1 . 1) and drops the 2.
   (let ((*duplicate-children-allowed-p* t))
     (call-next-method))))

(defmethod concrete-syntax-tree:reconstruct
    :around ((client client) (expression t) (cst t) &key default-source)
  (declare (ignore default-source))
  (let ((result (call-next-method)))
    ;; If RESULT has a CST source, it must be of type `wad'. This is
    ;; because `reconstruct' either makes a new non-wad CST node
    ;; without source information or pulls an existing node out of the
    ;; CST. The existing node was made via `make-expression-result'
    ;; etc, so it is of type `wad' if it source information was
    ;; available.
    (assert (or (null (cst:source result)) (typep result 'wad)))
    ;; If RESULT is a wad that is not already a child of some other
    ;; wad, then RESULT must be absolute with relative descendants.
    (when (and (typep result 'wad) (not (parent result)))
      (check-absolute-wad-with-relative-descendants result))
    ; (format *trace-output* "~&From reconstruct~%~2@T~A~%~2@T~A~%~2@T~A~%" expression cst default-source)
    ; (print-wad-tree result)
    ; (print-cst result)
    result))

(dbg:define-invariant make-error-wad
  :after (((error-wad 0))
          (assert (not (relative-p error-wad)))
          (assert (= (start-line error-wad) (absolute-start-line error-wad)))))

(dbg:define-invariant reader:read-maybe-nothing
  :after (((result 2))
          (unless (null result)
            (check-absolute-wad-with-relative-descendants result))))

(dbg:define-invariant read-and-cache
  :after (((result 1))
          (unless (null result)
            (assert (typep result 'wad))
            (check-absolute-wad-with-relative-descendants result))))

;;; Cache

(dbg:define-invariant pop-from-suffix
  :before (((cache 0))
           (let* ((old-suffix     (suffix cache))
                  (old-suffix-top (first old-suffix)))
             (assert (not (relative-p old-suffix-top))))))

(dbg:define-invariant push-to-prefix
  :before (((wad 1))
           (check-absolute-line-numbers wad)))

(defvar *pop-from-prefix-cache*)
(dbg:define-invariant pop-from-prefix
  :before (((cache 0))
           (setf *pop-from-prefix-cache* cache))
  :after  (()
           (let* ((cache          *pop-from-prefix-cache*)
                  (new-prefix-top (first (prefix cache))))
             (when (not (null new-prefix-top))
               (assert (not (relative-p new-prefix-top)))))
           (setf *pop-from-prefix-cache* nil)))

(dbg:define-invariant push-to-worklist
  :before (((wad 1))
           (check-absolute-wad-with-relative-descendants wad)))

(dbg:define-invariant push-to-residue
  :before (((wad 1))
           (check-absolute-wad-with-relative-descendants wad)))

(dbg:define-invariant pop-from-worklist
  :after (((result 0))
          (check-absolute-wad-with-relative-descendants result)))

(dbg:define-invariant adjust-worklist-and-suffix
  :before (((cache 0))
           (labels ((invalidate-children (wad)
                      (when (relative-p wad)
                        (setf (slot-value wad '%absolute-start-line) :invalid))
                      (map-children #'invalidate-children wad)))
             (mapc #'invalidate-children (worklist cache))
             (alexandria:when-let ((suffix (suffix cache)))
               (invalidate-children (first suffix))))))

(dbg:define-invariant cached-wad
  :after (((result 0))
          (when result
            (check-absolute-line-numbers result)
            (assert (null (parent result))))))

(let (cache)
  (dbg:define-invariant update
    :before (((analyzer 0))
             (setf cache (cache analyzer)))
    :after  (()
             (check-wad-graph cache))))

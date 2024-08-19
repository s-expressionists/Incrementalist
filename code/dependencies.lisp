(cl:in-package #:incrementalist)

;;; Invalidation

;;; Invalidate wads on the suffix (thus "following" the current parse
;;; location) that use CELL.
(defun invalidate-following-users (cell)
  (dbg:log :invalidation "      Invalidating following users of ~A~%" cell)
  (let (suffix residue worklist (following nil))
    (labels ((initialize (cache)
               (setf suffix   (suffix cache)
                     residue  (residue cache)
                     worklist (worklist cache))
               (when (and (null suffix) (null residue) (null worklist))
                 (return-from invalidate-following-users))
               (setf following (make-hash-table :test #'eq)))
             (compute-followingp (wad)
               (alexandria:if-let ((parent (parent wad)))
                 (followingp parent)
                 (when (or (relative-p wad) ; in rest of suffix
                           (eq wad (first suffix))
                           (find wad residue :test #'eq)
                           (find wad worklist :test #'eq))
                   t)))
             (followingp (wad)
               (alexandria:ensure-gethash
                wad following (compute-followingp wad)))
             (visit-user (user)
               ;; Initialize lazily and possibly exit early if there
               ;; are no following users.
               (when (null following)
                 (initialize (cache user)))
               (when (followingp user)
                 (dbg:log :invalidation "        Invalidating ~A~%" user)
                 (invalidate-wad user cell))))
      (declare (dynamic-extent #'initialize #'compute-followingp #'followingp
                               #'visit-user))
      (dep:map-users #'visit-user cell))))

;;; Cell updates

;;; Update the invalid WAD to make it valid again so that it can be
;;; reused.  To this end, replace cells on the `invalid' cell list of
;;; WAD with the current cell for the respective aspect.  Perform the
;;; update in descendants and parents of WAD as well.
(defun update-invalid-cells (wad)
  (loop :for old-cell :in (dep:invalid wad)
        :for aspect   =   (dep:aspect old-cell)
        :for new-cell =   (dep:find-cell aspect)
        :do (dbg:log :wad "  Updating cell ~A~%" old-cell)
            ;; Collect all descendants of WAD in which CELL is also
            ;; invalid then update OLD-CELL to NEW-CELL in all wads.
            (let ((descendants (make-array 1 :adjustable t :fill-pointer 0)))
              (labels ((visit (wad)
                         (when (find old-cell (dep:invalid wad) :test #'eq)
                           (vector-push-extend wad descendants)
                           (map-children #'visit wad))))
                (visit wad))
              (update-invalid-cell old-cell new-cell descendants))))

(defun update-invalid-cell (old-cell new-cell wads)
  (assert (dep:validp new-cell)) ; important when (eq old-cell new-cell)
  (labels ((update-use (wad)
             (let ((invalid (dep:invalid wad)))
               (when (find old-cell invalid :test #'eq)
                 (setf (dep:invalid wad) (delete old-cell invalid
                                                 :test #'eq :count 1))
                 ;; Update use relation in WAD from OLD-CELL to
                 ;; NEW-CELL if necessary.
                 (unless (eq old-cell new-cell)
                   (let ((used (dep:used wad)))
                     (when (find old-cell used :test #'eq)
                       (dep:remove-user wad old-cell)
                       (alexandria:deletef used old-cell :test #'eq :count 1)
                       (unless (find new-cell used :test #'eq)
                         (dep:add-user wad new-cell)
                         (setf used (list* new-cell used)))
                       (setf (dep:used wad) used))))
                 ;; The parent of WAD may have been invalidated without
                 ;; directly using OLD-CELL. We still have to remove
                 ;; OLD-CELL from the invalidation list of the parent.
                 (alexandria:when-let ((parent (parent wad)))
                   (update-use parent))))))
    (dbg:log :wad "  Replacing cell ~A -> ~A~%" old-cell new-cell)
    (map nil #'update-use wads)))

;;; Labeled objects state

(defmethod eclector.reader:find-labeled-object ((client client) (label t))
  (let* ((cell   (dep:find-cell 'labeled-objects))
         (value  (dep:value cell))
         (object (alexandria:assoc-value value label)))
    (assert (not (null cell)))
    (dep:register-use cell)
    (if (null object)
        (call-next-method)
        object)))

(defmethod eclector.reader:note-labeled-object ((client        client)
                                                (input-stream  t)
                                                (label         t)
                                                (parent        t))
  (let* ((values    (multiple-value-list (call-next-method)))
         (object    (first values))
         (parent    (dep:find-cell 'labeled-objects))
         (old-value (dep:value parent))
         (new-value (list* (cons label object) old-value)))
    (dep:make-and-register-cell
     'labeled-objects :top-level-expression new-value
     #'invalidate-following-users :parent parent)
    (values-list values)))

;;; Other state value

(defmethod eclector.reader:state-value ((client client) (aspect t))
  (let ((cell (dep:find-cell aspect)))
    (assert (not (null cell)))
    ;; When this is called from `call-as-top-level-read',
    ;; `dep:*used-cells*' is not bound yet since that happens in the
    ;; `read-maybe-nothing' method.
    (when (boundp 'dep:*used-cells*)
      (dep:register-use cell))
    (dep:value cell)))

(flet ((check-state-value-type (client aspect value)
         (unless (eclector.reader:valid-state-value-p client aspect value)
           (error 'eclector.reader:state-value-type-error
                  :stream        (stream* client)
                  :datum         value
                  :expected-type (case aspect ; TODO: get type from Eclector?
                                   (*read-base* '(integer 2 36))
                                   (t           nil))
                  :aspect        aspect))))

  (defmethod (setf eclector.reader:state-value) ((new-value t)
                                                 (client    client)
                                                 (aspect    t))
    (check-state-value-type client aspect new-value)
    (let* ((parent (dep:find-cell aspect))
           (scope  (dep:scope parent)))
      (dep:make-and-register-cell
       aspect scope new-value #'invalidate-following-users))
    new-value)

  (defmethod eclector.reader:call-with-state-value ((client client)
                                                    (thunk  t)
                                                    (aspect t)
                                                    (value  t))
    (check-state-value-type client aspect value)
    (let ((parent (dep:find-cell aspect)))
      (dep:make-and-register-cell
       aspect :lexical value #'invalidate-following-users :parent parent)
      (unwind-protect
           (funcall thunk)
        (dep:pop-cell-until aspect parent)))))

(cl:in-package #:incrementalist.dependencies)

;;; Scopes and aspects

(defun scope-string (scope)
  (ecase scope
    (:lexical              "v")
    (:top-level-expression ">")
    (:global               "+")))

(defun aspect-string (aspect)
  (check-type aspect symbol)
  (let ((name (symbol-name aspect)))
    (if (< (length name) 12)
        name
        (with-output-to-string (stream)
          (reduce (lambda (state char)
                    (case char
                      ((#\* #\-)
                       (write-char char stream)
                       :start)
                      (t
                       (when (eq state :start)
                         (write-char char stream))
                       :middle)))
                  name :initial-value :start)))))

(#+sbcl sb-ext:defglobal #-sbcl defvar **value-pprint-dispatch**
  (let ((table (copy-pprint-dispatch)))
    (set-pprint-dispatch '(or standard-object structure-object)
                         (lambda (stream object)
                           (declare (ignore object))
                           (write-string "#<..>" stream))
                         1 table)
    table))

(defun value-string (value)
  (with-output-to-string (stream)
    (let ((*print-length*          3)
          (*print-pprint-dispatch* **value-pprint-dispatch**))
      (prin1 value stream))))

;;; Class `cell' and cell protocol implementation

(defclass cell ()
  ((%aspect  :initarg  :aspect
             :reader   aspect)
   (%value   :initarg  :value
             :reader   value)
   ;; Objects (such as wads) that use the cell. Since there are often
   ;; no or few users, start with a compact representation and upgrade
   ;; as necessary.
   (%users   :accessor %users
             :type     (or null hash-table)
             :initform nil)
   ;; Validity
   (%validp  :accessor validp
             :type     boolean
             :initform t))
  (:default-initargs
   :aspect (a:required-argument :aspect)
   :value  (a:required-argument :value)))

(defmethod print-object ((object cell) stream)
  (print-unreadable-object (object stream :identity t)
    (let* ((validp        (validp object))
           (aspect        (aspect object))
           (scope-string  (scope-string (scope object)))
           (aspect-string (aspect-string aspect))
           (value         (value object))
           (user-count    (length (users object))))
      (format stream "~A ~:[INVALID ~;~]~A ~A → "
              (symbol-name 'cell) validp scope-string aspect-string)
      (let ((*print-length*          3)
            (*print-pprint-dispatch* **value-pprint-dispatch**))
        (prin1 value stream))
      (format stream " (~D user~:P)" user-count))))

(defmethod map-users ((function t) (cell cell))
  (a:when-let ((users (%users cell)))
    (let ((function (alexandria:ensure-function function)))
      (a:maphash-values function users))))

(defmethod users ((cell cell))
  (a:if-let ((users (%users cell)))
    (a:hash-table-values users)
    nil))

(defmethod add-user ((user t) (cell cell))
  (let ((users (%users cell)))
    (when (null users)
      (setf users         (make-hash-table :test #'eq)
            (%users cell) users))
    (setf (gethash user users) user)))

(defmethod remove-user ((user t) (cell cell))
  (remhash user (%users cell)))

(defmethod remove-user ((user vector) (cell cell))
  (loop :with users = (%users cell)
        :for user* :across user
        :do (remhash user* users)))

(macrolet ((define (class-name scope)
             `(progn
                (defclass ,class-name (cell) ())

                (defmethod scope ((cell ,class-name)) ',scope))))
  (define globally-scoped-cell             :global)
  (define top-level-expression-scoped-cell :top-level-expression)
  (define lexically-scoped-cell            :lexical))

(declaim (inline make-cell))
(defun make-cell (scope aspect value)
  (ecase scope
    (:global
     (make-instance 'globally-scoped-cell :aspect aspect :value value))
    (:top-level-expression
     (make-instance 'top-level-expression-scoped-cell
                    :aspect aspect :value value))
    (:lexical
     (make-instance 'lexically-scoped-cell :aspect aspect :value value))))

;;; Class `cell-user-mixin'

(deftype object-or-list (list)
  `(or null standard-object ,list))

(defclass cell-user-mixin ()
  (;; Either `nil' if the wad is valid, `t' if the wad has been
   ;; explicitly invalidated or a list of invalid cells that is a
   ;; subset of the used cells. In the latter case, the wad may or may
   ;; not be restored from the cache and reused depending on whether
   ;; the invalid cells have been replaced by new cells with
   ;; compatible values.
   (%invalid     :accessor invalid
                 :type     (or (eql t) #+incrementalist-debug list-of-cells
                                       #-incrementalist-debug list)
                 :initform '())
   ;; Cells used in this wad: invalidation of any of the listed cells
   ;; may cause the wad to become invalid.
   (%used        :accessor used
                 :type     #+incrementalist-debug list-of-cells-for-distinct-aspects
                           #-incrementalist-debug list
                 :initform '())
   ;; Cells directly defined in this wad.
   (%defined     :accessor %defined
                 :type     (object-or-list #+incrementalist-debug list-of-cells
                                           #-incrementalist-debug list)
                 :initform nil)
   ;; Cells that are defined in descendants of the wad and can affect
   ;; wads which follow this wad.
   (%escaped     :accessor %escaped
                 :type     (object-or-list
                            #+incrementalist-debug list-of-cells-for-distinct-aspects
                            #-incrementalist-debug list)
                 :initform nil)
   ;; This slot is used only when the wad is at the top-level.  Cells
   ;; inherited from the previous top-level wad or the initial
   ;; environment.  The set of cells can intersect the sets in the
   ;; USED and ESCAPED slots (in terms of both, cell objects and
   ;; "covered" aspects).
   ;;
   ;; Note: The value is potentially shared between users and thus
   ;; must not be destructively modified.
   (%inherited   :accessor inherited
                 :type     (or (eql :invalid)
                               #+incrementalist-debug list-of-cells-for-distinct-aspects
                               #-incrementalist-debug list)
                 :initform '())))

(defmethod validp ((object cell-user-mixin))
  (null (invalid object)))

(macrolet ((%map (function defined)
             (alexandria:once-only (defined)
               `(typecase ,defined
                  (null)
                  (cons (mapc ,function ,defined))
                  (t    (funcall ,function ,defined)))))
           (do-external ((cell-var external-scopes defined escaped) &body body)
             `(labels ((do-it (,cell-var)
                         ,@body)
                       (consider (cell)
                         (let ((scope (scope cell)))
                           (when ,(if (keywordp external-scopes)
                                      `(eq scope ,external-scopes)
                                      `(find scope ,external-scopes
                                             :test #'eq))
                             (do-it cell)))))
                (declare (dynamic-extent #'do-it #'consider))
                (%map #'consider ,defined)
                (%map #'do-it ,escaped))))

  (defmethod map-defined ((function t) (object cell-user-mixin))
    (%map function (%defined object)))

  (defmethod defined ((object cell-user-mixin))
    (let ((defined (%defined object)))
      (typecase defined
        (list defined)
        (t    (list defined)))))

  (defmethod add-defined ((defined cell) (object cell-user-mixin))
    (let ((old-defined (%defined object)))
      (setf (%defined object) (typecase old-defined
                                (null defined)
                                (cons (list* defined old-defined))
                                (t    (list defined old-defined))))))

  (defmethod add-defined ((defined list) (object cell-user-mixin))
    (let* ((old-defined (%defined object))
           (new-defined (typecase old-defined
                          (null defined)
                          (cons (nconc old-defined defined))
                          (t    (list* old-defined defined)))))
      (setf (%defined object) (typecase new-defined
                                (null          '())
                                ((cons t null) (first new-defined))
                                (t             new-defined)))))

  (defmethod map-escaped ((function t) (object cell-user-mixin))
    (%map function (%escaped object)))

  (defmethod escaped ((object cell-user-mixin))
    (let ((escaped (%escaped object)))
      (typecase escaped
        (list escaped)
        (t    (list escaped)))))

  (defmethod (setf escaped) ((new-value list) (object cell-user-mixin))
    (setf (%escaped object) (typecase new-value
                              ((cons t null) (first new-value))
                              (t             new-value)))
    new-value)

  (defmethod map-external ((function t) (object cell-user-mixin)
                           &key (external-scopes '(:global
                                                   :top-level-expression)))
    (let ((function (alexandria:ensure-function function)))
      (do-external (cell external-scopes (%defined object) (%escaped object))
        (funcall function cell))))

  ;; Only for top-level objects
  (defmethod inheritable ((object cell-user-mixin))
    (let ((defined (%defined object))
          (escaped (%escaped object)))
      (if (and (null defined) (null escaped))
          (inherited object)
          (let ((result (copy-list (inherited object))))
            (do-external (cell :global defined escaped)
              (setf result (nsubstitute cell (aspect cell) result
                                        :test #'eq :key #'aspect)))
            result)))))

(defun add-dependencies (object defined-cells used-cells escaping-cells)
  (unless (null defined-cells)
    (add-defined defined-cells object))
  (unless (null used-cells)
    (alexandria:nconcf (used object) used-cells)
    (loop :for cell :in used-cells
          :do (add-user object cell)))
  (unless (null escaping-cells)
    (setf (escaped object) escaping-cells)))

(defun update-escaping-for-toplevel-user (user invalidate-users)
  (let ((escaping '()))
    (map-escaped (lambda (cell)
                   (if (eq (scope cell) :global)
                       (push cell escaping)
                       (progn
                         (dbg:log :invalidate "  ~A reached toplevel, invalidating following users~%" cell)
                         (funcall invalidate-users cell))))
                 user)
    (dbg:log :register "  For ~A, reducing escaping~%  from ~:A~%  to   ~:A~%"
             user (escaped user) escaping)
    (setf (escaped user) (nreverse escaping))))

;;; Lexical state protocol

;;; Alist is faster than hash-table for maybe seven or eight distinct
;;; aspects when the frequently accessed aspects are at the front of
;;; the list.
(declaim (type list *cells*))
(defvar *cells*)

(defmacro with-cells (() &body body)
  `(let ((*cells* '())) ,@body))

(declaim (inline find-cell push-cell pop-cell))

(defun find-cell (aspect)
  (first (cdr (assoc aspect *cells* :test #'eq))))

(defun push-cell (cell aspect)
  (let* ((cells *cells*)
         (entry (or (assoc aspect cells :test #'eq)
                    (let ((entry (cons aspect '())))
                      (setf *cells* (nconc cells (list entry)))
                      entry))))
    (push cell (cdr entry))))

(defun pop-cell (aspect)
  (pop (cdr (assoc aspect *cells* :test #'eq))))

(defun pop-cell-until (aspect new-top-cell)
  (loop :with entry = (assoc aspect *cells* :test #'eq)
        :for stack = (cdr entry) :then rest
        :for (first . rest) = stack
        :when (eq first new-top-cell)
          :do (setf (cdr entry) stack)
              (loop-finish)))

(defun pop-local-cells ()
  (loop :for entry :in *cells*
        :for (nil . stack) = entry
        :for cell = (first stack)
        :for scope = (scope cell)
        :when (eq scope :top-level-expression)
          ;; Remove all but global value of the cell.
          :do (setf (cdr entry) (last stack))))

;;; Restoring current reader state

(defun install-state (cells)
  (dbg:log :state "Initial state~%")
  (loop :for cell :in cells
        :do (dbg:log :state "  ~10A ~A -> ~A~%"
                     :initial (aspect cell) (value-string (value cell)))
            (push-cell cell (aspect cell))))

(defun install-state-from-user (user toplevelp)
  (dbg:log :state "State from ~A~%" user)
  (let ((aspects (make-array 16 :adjustable nil :fill-pointer 0)))
    (labels ((install-cell (cell where-from)
               (assert (validp cell))
               (if (case (scope cell)
                     (:lexical              nil)
                     (:top-level-expression (not toplevelp))
                     (:global               t))
                   (let ((aspect (aspect cell)))
                     (dbg:log :state "  ~10A ~A -> ~A~%"
                              where-from aspect (value-string (value cell)))
                     (assert (not (find aspect aspects :test #'eq)))
                     (vector-push aspect aspects)
                     (push-cell cell aspect))
                   (dbg:log :state "  ~10A Not using ~A, valid ~A, toplevel ~A~%"
                            where-from cell (validp cell) toplevelp)))
             (maybe-install-cell (cell where-from)
               (unless (find (aspect cell) aspects :test #'eq)
                 (install-cell cell where-from))))
      ;; Escaped cells must take precedence over defined cells in situations
      ;; such as
      ;;
      ;;  #1=(#2=foo #3=bar)
      ;;
      ;; in which the value of the defined cell will be ((1 . #<..>)) but the
      ;; value of the escaped cell will be
      ;; ((3 . #<..>) (2 . #<..>) (1 . #<..>)).
      (map-escaped (lambda (cell) (install-cell cell :escaped)) user)
      (map-defined (lambda (cell) (maybe-install-cell cell :defined)) user)
      (mapc (lambda (cell) (maybe-install-cell cell :inherited))
            (inherited user)))))

(defmethod valid-for-state-p ((object cell-user-mixin))
  (let ((invalid (invalid object)))
    (and (listp invalid) ; When INVALID is `t', OBJECT is always invalid
         (loop :for cell :in invalid
               :for aspect = (aspect cell)
               :for state-cell = (find-cell aspect)
               :always (and state-cell
                            (equal (value cell) (value state-cell)))))))

;;; Cell definition and use protocol

;;; List of cells that are being newly defined in the scope of the current
;;; `read-maybe-nothing' call.
(declaim (type list *defined-cells*))
(defvar *defined-cells*)

;;; List of cells that have been used in the scope of the current
;;; `read-maybe-nothing' call.
(declaim (type list *used-cells*))
(defvar *used-cells*)

;;; List of cells that have been defined in the scope of the current
;;; `read-maybe-nothing' call or descendant `read-maybe-nothing' calls and may
;;; affect the results of subsequent `read-maybe-nothing' calls.
(declaim (type #+incrementalist-debug list-of-cells-for-distinct-aspects
               #-incrementalist-debug list
               *escaping-cells*))
(defvar *escaping-cells*)

(defun register-definition (new-cell parent invalidate-users)
  ;; When the new definition NEW-CELL is inserted between the
  ;; definition PARENT and uses of PARENT, those uses may become
  ;; invalid since they may have to use NEW-CELL instead of
  ;; PARENT. `invalidate-users' must take care of this invalidation.
  (dbg:log :invalidation "  Registering definitions~%")
  (when (not (null parent))
    (dbg:log :invalidation "    Defined ~A, invalidating users~%" new-cell)
    (funcall invalidate-users parent))
  (pushnew new-cell *defined-cells* :test #'eq))

;;; Declaimed INLINE so that for constant SCOPE, when MAKE-CELL is inlined, the
;;; MAKE-INSTANCE call for the cell sees a constant class.
(declaim (inline make-and-register-cell))
(defun make-and-register-cell (aspect scope value invalidate-users
                               &key (parent (find-cell aspect)))
  (let ((cell (make-cell scope aspect value)))
    (register-definition cell parent invalidate-users)
    (push-cell cell aspect)
    cell))

(declaim (inline register-use))
(defun register-use (cell)
  (pushnew cell *used-cells* :test #'eq))

(defun register-escaping (definer)
  (dbg:log :register "  Registering escaping cells~%")
  ;; The variable is bound if the `read' call which calls this
  ;; function is surrounded by another read call (the cells which
  ;; escape from the inner call escape to the outer call).
  (when (boundp '*escaping-cells*)
    (let ((escaping *escaping-cells*))
      (flet ((visit-cell (cell)
               (dbg:log :register "    ~A~%" cell)
               ;; ESCAPING may already contain a cell for the aspect
               ;; of CELL for two reasons: 1) a previous call to
               ;; `register-escaping' 2) DEFINER may contain multiple
               ;; definitions for the same aspect.
               (let ((other (delete (aspect cell) escaping
                                    :test #'eq :key #'aspect :count 1)))
                 (setf escaping (list* cell other)))))
        (declare (dynamic-extent #'visit-cell))
        (map-external #'visit-cell definer))
      (setf *escaping-cells* escaping))))

(defmacro with-cell-dependencies ((add-dependencies-name) &body body)
  `(let ((*defined-cells*  '())
         (*used-cells*     '())
         (*escaping-cells* '()))
     (flet ((,add-dependencies-name (object)
              (add-dependencies
               object *defined-cells* *used-cells* *escaping-cells*)))
       ,@body)))

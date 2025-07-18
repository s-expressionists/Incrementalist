(cl:in-package #:incrementalist)

;;;                     wad
;;;  ┌───────────────────┴────────┬───────┐
;;;  │             cst            │       │
;;;  │              │             │       │
;;;  │      ┌───────┴───────┐     │       │
;;;  │  cons-cst        atom-cst  │       │
;;;  │    │                  │    │       │
;;; cons-wad                atom-wad  skipped-wad

;;; `no-children-mixin'
;;;
;;; This class is intended to be mixed into wad classes the instances
;;; of which never have children.

(defclass no-children-mixin () ())

(defmethod children ((wad no-children-mixin))
  '())

(defmethod map-children ((function t) (object no-children-mixin)))

;;; `maybe-extra-children-mixin' and `extra-children-mixin'

(defclass maybe-extra-children-mixin ()
  ((%children :initarg  :children
              :type     list
              :reader   children))
  (:default-initargs
   :children (alexandria:required-argument :children)))

(defmethod map-children ((function t) (object maybe-extra-children-mixin))
  (mapc function (children object)))

(defclass extra-children-mixin (maybe-extra-children-mixin)
  ((%children :type     cons))) ; must not be used if child list would be empty

;;; `family-relations-mixin'

(defclass family-relations-mixin ()
  (;; This slot contains the parent wad of this wad, or NIL if this
   ;; wad is a top-level wad.
   (%parent        :initarg  :parent
                   :accessor parent
                   :initform nil)
   ;; This slot contains the left sibling wad of this wad, or NIL if
   ;; this wad is the first child of its parent.  If this wad is a
   ;; top-level wad, then this slot contains the preceding top-level
   ;; wad, or NIL if this is the first top-level wad in the buffer.
   (%left-sibling  :initarg  :left-sibling
                   :accessor left-sibling
                   :initform nil)
   ;; This slot contains the right sibling wad of this wad, or NIL if
   ;; this wad is the last child of its parent.  If this wad is a
   ;; top-level wad, then this slot contains the following top-level
   ;; wad, or NIL if this is the last top-level wad in the buffer.
   (%right-sibling :initarg  :right-sibling
                   :accessor right-sibling
                   :initform nil)))

;;; A WAD is the result of parsing an expression or some material that
;;; is normally skipped, such as a comment or an inactive reader
;;; conditional.
(defclass basic-wad ()
  (;; This slot contains the cache that this wad is part of.
   (%cache               :initarg  :cache
                         :reader   cache)
   ;; This slot contains TRUE if and only if the START-LINE slot is
   ;; relative to some other line.
   (%relative-p          :initarg :relative-p
                         :accessor relative-p)
   ;; This slot contains the absolute start line of the wad.  Its
   ;; contents is valid only in certain situations, for example, when
   ;; the wad is on the prefix, and when the wad is the top-level wad
   ;; which is the first on the suffix.  With a wad in a different
   ;; place, this slot may contain some obsolete value.  When
   ;; assertions are enabled, we define a :BEFORE method on the slot
   ;; reader to detect some of those situations and prevent the value
   ;; from being read in those cases.
   (%absolute-start-line :initarg  :absolute-start-line
                         :type     #+incrementalist-debug (or (eql :invalid) (integer 0))
                                   #-incrementalist-debug (integer 0)
                         :accessor absolute-start-line)
   ;; This slot contains information about the start line of the wad.
   ;; Simple applications might always store the absolute line number
   ;; of the first line of the wad in this slot.  Other applications
   ;; might store a line number relative to some other wad.
   (%start-line          :initarg  :start-line
                         :type     (integer 0)
                         :accessor start-line)
   ;; This slot contains the difference between the start line and the
   ;; end line.  A value of 0 indicates that the wad starts and ends
   ;; in the same line.
   (%height              :initarg  :height
                         :type     (integer 0)
                         :reader   height)
   ;; This slot contains the absolute column of the first character in
   ;; this wad.  A value of 0 indicates that this wad starts in the
   ;; leftmost position in the source code.
   (%start-column        :initarg  :start-column
                         :type     (integer 0)
                         :accessor start-column)
   ;; This slot contains the absolute column of the last character of
   ;; the wad.  The value of this slot can never be 0.  If the last
   ;; character of the wad is the leftmost character in a line, then
   ;; this slot contains the value 1.
   (%end-column          :initarg  :end-column
                         :type     (integer 0) ; TODO despite what the comment says, 0 is used in some cases
                         :accessor end-column)))

(defun print-wad-position (wad stream)
  (format stream "~:[abs~;rel~]:~d[~d],~d -> ~d,~d"
          (relative-p wad)
          (start-line wad)
          (when (slot-boundp wad '%absolute-start-line)
            (slot-value wad '%absolute-start-line))
          (start-column wad)
          (if (relative-p wad)
              (height wad)
              (end-line wad))
          (end-column wad)))

(defmethod print-object ((object basic-wad) stream)
  (print-unreadable-object (object stream :type t)
    (print-wad-position object stream)))

(defmethod end-line ((wad basic-wad))
  (assert (not (relative-p wad)))
  (+ (start-line wad) (height wad)))

(defclass wad (dep:cell-user-mixin family-relations-mixin basic-wad)
  (;; This slot contains the maximum line width of any line that is
   ;; part of the wad.
   (%max-line-width :initarg :max-line-width
                    :reader  max-line-width)
   ;; This slot stores different kinds of errors which are represented
   ;; as `error-wad's. Character syntax errors as reported by Eclector
   ;; are stored in the "closest surrounding" wad. S-expression syntax
   ;; errors are stored in the containing top-level wad. Error wads
   ;; are always relative. A single error is stored without the
   ;; surrounding list. A value of `t' is used for misspelled words.
   (%errors         :type     (or (eql t) basic-wad list)
                    :accessor %errors
                    :initform '())
   ;; This slot contains the absolute column that the first character
   ;; of this wad should be positioned in, as computed by the rules of
   ;; indentation.  If this wad is not the first one on the line, then
   ;; this slot contains NIL.
   (%indentation    :initarg  :indentation
                    :accessor indentation
                    :initform nil)))

(defmethod print-object ((object wad) stream)
  (print-unreadable-object (object stream :type t)
    (unless (dep:validp object)
      (write-string "INVALID " stream))
    (print-wad-position object stream)))

(defmethod errors ((wad wad))
  (let ((errors (%errors wad)))
    (typecase errors
      (list errors)
      (t    (list errors)))))

(declaim (inline set-errors))
(defun set-errors (wad reversed-errors)
  (unless (null reversed-errors)
    (let ((errors (nreverse reversed-errors)))
      (loop :for base = (start-line wad) :then start-line
            :for error-wad :in errors
            :for start-line = (absolute-to-relative error-wad base))
      (setf (%errors wad) (if (null (cdr errors))
                              (first errors)
                              errors)))))

(defun add-error (wad error)
  (assert (not (relative-p error)))
  (let ((old-errors (%errors wad)))
    (absolute-to-relative error (absolute-start-line wad))
    (setf (%errors wad)
          (cond ((null old-errors)
                 error)
                ((not (consp old-errors))
                 (if (wad-starts-before-wad-p error old-errors)
                     (list error old-errors)
                     (list old-errors error)))
                (t
                 (merge 'list (list error) old-errors
                        #'wad-starts-before-wad-p))))))

(declaim (inline link-siblings))
(defun link-siblings (left right)
  (unless (null left)
    (setf (right-sibling left) right))
  (unless (null right)
    (setf (left-sibling right) left)))

(defun make-children-relative-and-set-family-relations (wad)
  (let ((previous nil)
        (base     (start-line wad)))
    (flet ((adjust-child (child)
             ;; Set relations
             (setf (parent child) wad)
             (link-siblings previous child)
             ;; Make relative
             (setf base (absolute-to-relative child base))
             (setf previous child)))
      (declare (dynamic-extent #'adjust-child))
      (map-children #'adjust-child wad)
      (link-siblings previous nil)))
  wad)

(defgeneric relative-to-absolute (wad offset)
  (:method ((wad wad) offset)
    (assert (relative-p wad))
    (let ((new-start-line (+ (start-line wad) offset)))
      (setf (start-line          wad) new-start-line
            (absolute-start-line wad) new-start-line
            (relative-p          wad) nil
            (parent              wad) nil))))

(defgeneric absolute-to-relative (wad offset)
  (:method ((wad basic-wad) offset)
    (assert (not (relative-p wad)))
    (setf (relative-p wad) t)
    (let ((absolute-start-line (start-line wad)))
      (setf (start-line wad) (- absolute-start-line offset))
      absolute-start-line)))

;;; RELATIVE-WADS is a list of wads where the start line of the first
;;; element is relative to OFFSET, and the start line of each of the
;;; other elements is relative to the start line of the preceding
;;; element.  Modify the wads in the list so that they are absolute.
;;; Return the original list, now containing the modified wads.
(defun make-absolute (relative-wads offset)
  (loop with base = offset
        for wad in relative-wads
        do (relative-to-absolute wad base)
           (setf base (start-line wad)))
  relative-wads)

;;; ABSOLUTE-WADS is a list of absolute wads.  Modify the wads in the
;;; list so that the start line of the first element is absolute to
;;; OFFSET, and the start line of each of the other elements is
;;; relative to the start line of the preceding element.  Return the
;;; original list, now containing the modified wads.
(defun make-relative (absolute-wads offset)
  (loop for base = offset then start-line
        for wad in absolute-wads
        for start-line = (absolute-to-relative wad base))
  absolute-wads)

(defun compute-absolute-line-numbers (top-level-wad)
  ;; Make sure TOP-LEVEL-WAD itself is absolute, so that we need to
  ;; compute the absolute line numbers only of its descendants.
  (assert (not (relative-p top-level-wad)))
  (labels ((process-children (parent offset)
             (let ((base offset))
               (flet ((process-child (child)
                        (let* ((start-line          (start-line child))
                               (absolute-start-line (+ base start-line)))
                          (declare (type alexandria:array-index
                                         start-line absolute-start-line))
                          (setf (absolute-start-line child)
                                absolute-start-line)
                          (process-wad child absolute-start-line)
                          (setf base absolute-start-line))))
                 (declare (dynamic-extent #'process-child))
                 (map-children #'process-child parent))))
           (process-wad (wad wad-start-line)
             (process-children wad wad-start-line)
             (loop for base = wad-start-line then absolute-start-line
                   for error-wad in (errors wad)
                   for start-line of-type alexandria:array-index
                      = (start-line error-wad)
                   for absolute-start-line = (+ base start-line)
                   do (setf (absolute-start-line error-wad)
                            absolute-start-line))))
    (let ((start-line (start-line top-level-wad)))
      (declare (type alexandria:array-index start-line))
      (setf (absolute-start-line top-level-wad) start-line)
      (process-wad top-level-wad start-line)))
  top-level-wad)

(defmethod items ((wad basic-wad))
  (with-output-to-string (stream) ; assume character items for now
    (loop with cache                                     =    (cache wad)
          with start-line of-type alexandria:array-index =    (absolute-start-line wad)
          for i           of-type (or (eql -1) alexandria:array-index) from (height wad) downto 0
          for line-number of-type alexandria:array-index from start-line
          for line        of-type simple-string          =    (line-contents cache line-number)
          for start-column                               =    (start-column wad) then 0
          for end-column                                 =    (if (zerop i)
                                                                  (end-column wad)
                                                                  (length line))
          do (write-string line stream :start start-column :end end-column)
          unless (zerop i)
            do (write-char #\Newline stream))))

;;; During parsing, this variable holds the cache to which all created
;;; wads belong.
(defvar *cache*)

;;; Return true if and only if the position indicated by
;;; RELATIVE-LINE-NUMBER and COLUMN-NUMBER is entirely before WAD.  If
;;; WAD is an absolute wad, then RELATIVE-LINE-NUMBER must be the
;;; absolute line number of the position.  If WAD is a relative wad,
;;; then RELATIVE-LINE-NUMBER must be the difference between the
;;; absolute line number of the position, and the start line of the
;;; wad to which WAD is relative.  The position is before WAD if
;;; either RELATIVE-LINE-NUMBER is strictly less than the start line
;;; of WAD, or if RELATIVE-LINE-NUMBER is equal to the start line of
;;; WAD, and COLUMN-NUMBER is less than or equal to the start column
;;; of WAD.
(defun position-is-before-wad-p (wad relative-line-number column-number)
  (%position<= relative-line-number column-number
               (start-line wad) (start-column wad)))

;;; Return true if and only if the position indicated by
;;; RELATIVE-LINE-NUMBER and COLUMN-NUMBER is entirely after WAD.  If
;;; WAD is an absolute wad, then RELATIVE-LINE-NUMBER must be the
;;; absolute line number of the position.  If WAD is a relative wad,
;;; then RELATIVE-LINE-NUMBER must be the difference between the
;;; absolute line number of the position, and the start line of the
;;; wad to which WAD is relative.  The position is after WAD if either
;;; RELATIVE-LINE-NUMBER is strictly greater than the sum of the start
;;; line of WAD and the height of WAD, or if RELATIVE-LINE-NUMBER is
;;; equal to the sum of the start line of WAD and the height of WAD,
;;; and COLUMN-NUMBER is greater than or equal to the end column of
;;; WAD.
(defun position-is-after-wad-p (wad relative-line-number column-number)
  (%position>= relative-line-number column-number
               (+ (start-line wad) (height wad)) (end-column wad)))

;;; Return true if and only if the position indicated by
;;; RELATIVE-LINE-NUMBER and COLUMN-NUMBER is inside WAD.  If WAD is
;;; an absolute wad, then RELATIVE-LINE-NUMBER must be the absolute
;;; line number of the position.  If WAD is a relative wad, then
;;; RELATIVE-LINE-NUMBER must be the difference between the absolute
;;; line number of the position, and the start line of the wad to
;;; which WAD is relative.  The position is inside WAD if it is
;;; neither before WAD nor after WAD.
(declaim (inline position-is-inside-wad-p))
(defun position-is-inside-wad-p (wad relative-line-number column-number
                                 &key (start-relation '<)
                                      (end-relation   '<))
  (let ((start-line (start-line wad)))
    (macrolet ((one-boundary (relation &rest arguments)
                 `(ecase ,relation
                    (<  (%position< ,@arguments))
                    (<= (%position<= ,@arguments)))))
      (and (one-boundary start-relation
                         start-line                  (start-column wad)
                         relative-line-number        column-number)
           (one-boundary end-relation
                         relative-line-number        column-number
                         (+ start-line (height wad)) (end-column wad))))))

(defun wad-starts-before-wad-p (wad1 wad2)
  (%position< (start-line wad1) (start-column wad1)
              (start-line wad2) (start-column wad2)))

(defun wad-ends-after-wad-p (wad1 wad2)
  (not (%position< (end-line wad2) (end-column wad2)
                   (end-line wad1) (end-column wad1))))

(defun wad-contains-wad-p (wad1 wad2)
  (and (wad-starts-before-wad-p wad1 wad2)
       (wad-ends-after-wad-p wad1 wad2)))

;;; Dependencies and invalidation

(defvar *drop-depth* 0)
(defmethod detach-wad ((wad basic-wad) &key (recursive t))
  (if recursive
      (labels ((rec (wad)
                 (%detach-wad wad)
                 (let ((*drop-depth* (+ *drop-depth* 2)))
                   (map-children #'rec wad))))
        (rec wad))
      (%detach-wad wad)))

(defun %detach-wad (wad)
  (dbg:log :invalidation "~V<~>Dropping wad ~A~%"
           (* 2 *drop-depth*) wad)
  (let ((*drop-depth* (1+ *drop-depth*)))
    (dep:map-defined #'invalidate-cell wad)
    (loop :for cell :in (dep:used wad)
          :when (dep:validp cell)
            :do (dbg:log :invalidation "~V<~>USE ~A~%" (* 2 *drop-depth*) cell)
                (dep:remove-user wad cell))))

(defun invalidate-cell (cell)
  (dbg:log :invalidation "~V<~>DEF ~A~%" (* 2 *drop-depth*) cell)
  (setf (dep:validp cell) nil)
  (let ((*drop-depth* (1+ *drop-depth*)))
    (dep:map-users (lambda (wad)
                     (invalidate-wad wad cell))
                   cell)))

(defun invalidate-wad (wad cell)
  (labels ((rec (wad)
             (dbg:log :invalidation "~V<~>Invalidate ~A~%" (* 2 *drop-depth*) wad)
             (let ((old-invalid (dep:invalid wad)))
               (unless (find cell old-invalid :test #'eq)
                 (setf (dep:invalid wad) (list* cell old-invalid))
                 (alexandria:when-let ((parent (parent wad)))
                   (let ((*drop-depth* (1+ *drop-depth*)))
                     (rec parent)))))))
    (rec wad)))

(defun state-aspect-cell (wad aspect)
  (labels ((rec (wad)
             (or (find aspect (dep:inherited wad) :test #'eq :key #'dep:aspect)
                 (alexandria:when-let ((parent (parent wad)))
                   (rec parent)))))
    ;; TODO: Maybe we should have a parameter to indicate whether we
    ;; want incoming or outgoing values
    (or (find aspect (dep:used wad) :test #'eq :key #'dep:aspect)
        (rec wad))))

(defun state-value (wad aspect)
  (alexandria:if-let ((cell (state-aspect-cell wad aspect)))
    (values (dep:value cell) t)
    (values nil              nil)))

;;; CST wads
;;;
;;; CST wads contain a "raw" expression. Note that wads based on
;;; `cst:atom-cst' and `cst:cons-cst' are not the only CST wads. One
;;; other example are labeled object definitions and references which
;;; are based `eclector.concrete-syntax-tree:wrapper-cst'.

(defclass cst-wad (wad cst:cst) ; TODO we inherit the `%source' slot which we do not use
  ())

;;; Atom

;;; "Plain" `atom-cst' instances do not have any children.
(defmethod map-children ((function t) (wad cst:atom-cst)))

(defmethod children ((wad cst:atom-cst))
  '())

(defclass atom-wad (cst-wad cst:atom-cst)
  ())

(defmethod print-object ((object atom-wad) stream)
  (print-unreadable-object (object stream :type t)
    (print-wad-position object stream)
    (let* ((raw        (cst:raw object))
           (raw-string (typecase raw
                         #+(or sbcl ccl)
                         (string
                          (substitute #\Pilcrow_Sign #\Newline raw))
                         (t
                          raw))))
     (format stream " raw: ~S" raw-string))))

(defmethod cst:null ((cst atom-wad))
  (let ((raw (cst:raw cst)))
    (and (typep raw 'symbol-token)
         (eq (name         raw) **nil-symbol-name**)
         (eq (package-name raw) **common-lisp-package-name**))))

(defmethod detach-wad ((wad atom-wad) &key recursive)
  (declare (ignore recursive))
  (call-next-method)
  (when (stringp (cst:raw wad))
    ;; The children are `text-wad's so they do not define any cells
    ;; and do not have descendants of their own.
    (map-children (lambda (child) (setf (dep:invalid child) t)) wad)))

(defclass atom-wad-with-extra-children (extra-children-mixin atom-wad)
  ())

;;; Cons

;;; Children of "plain" `cons-cst' instances are defined by the CST
;;; structure.
(defmethod map-children ((function t) (wad cst:cons-cst))
  (let ((function (alexandria:ensure-function function)))
    (labels ((visit-wad-child (child expected-parent)
               (when (let ((parent (parent child)))
                       (or (null parent) (eq parent expected-parent)))
                 (funcall function child)))
             (visit-maybe-wad-child (child parent)
               ;; It is possible that CHILD is a CST child (that is
               ;; `cst:first' or `cst:rest') of PARENT but not a WAD
               ;; child. This can happen due to `cst:reconstruct' in
               ;; cases like the invalid expression ,(a b) since the
               ;; whole expression is represented as a `cons-wad' with
               ;; one `atom-wad' child for symbol a and a `cons-cst'
               ;; child for (b). However, on the WAD level, the top
               ;; `cons-wad' contains an intermediate `cons-wad' which
               ;; represents (a b).
               (when (typep child 'wad)
                 (visit-wad-child child parent)))
             (visit-cons (node wad-parent dotted?)
               (visit-maybe-wad-child (cst:first node) wad-parent)
               (when dotted?
                 (visit-maybe-wad-child (cst:rest node) wad-parent)))
             (visit-atom (node)
               ;; NODE is a wad iff WAD represents a dotted list since
               ;; otherwise NODE represents an implicit `nil' as a
               ;; `atom-cst' instance which is not a wad.
               (when (typep node 'wad)
                 (funcall function node))))
      (declare (inline visit-wad-child visit-maybe-wad-child))
      ;; Walk along the list structure of which WAD is the head. Call
      ;; FUNCTION for each list element that is of type `wad'. The
      ;; list structure may be proper or dotted. In the latter case,
      ;; call FUNCTION for the wad that is the tail as well.
      (loop for node = wad then (cst:rest node)
            if (cst:consp node)
              do (let* ((rest       (cst:rest node))
                        (dotted?    (not (or (cst:consp rest) (cst:null rest))))
                        (wad-child? (and (not (eq node wad)) (typep node 'wad))))
                   (if wad-child?
                       (funcall function node)
                       (visit-cons node wad dotted?))
                   (when (or dotted? wad-child?)
                     (loop-finish)))
            else ; must be `atom-cst'
              do (visit-atom node)
                 (loop-finish)))))

(defclass cons-wad (cst-wad cst:cons-cst)
  ())

(defclass cons-wad-with-extra-children (extra-children-mixin cons-wad)
  ())

;;; Definition and reference wads

(defclass labeled-object-wad (cst-wad)
  ())

(defclass labeled-object-definition-wad (extra-children-mixin
                                         labeled-object-wad
                                         eclector.concrete-syntax-tree:definition-cst)
  ())

(defclass labeled-object-reference-wad (no-children-mixin
                                        labeled-object-wad
                                        eclector.concrete-syntax-tree:reference-cst)
  ())

;;; Non-CST wads

(defclass non-cst-wad (wad)
  ())

(defclass skipped-wad (non-cst-wad)
  ())

;;; This class is the base class of all comment wads.
(defclass comment-wad (maybe-extra-children-mixin skipped-wad)
  ())

(defmethod detach-wad ((wad comment-wad) &key recursive)
  (declare (ignore recursive))
  (%detach-wad wad)
  ;; The children are `text-wad's so they do not define any cells and
  ;; do not have descendants of their own.
  (map-children (lambda (child) (setf (dep:invalid child) t)) wad))

;;; This class is used for a block comment introduced by #|.
(defclass block-comment-wad (comment-wad)
  ())

;;; This class is used for a comment introduced by one or more
;;; semicolons.
(defclass semicolon-comment-wad (comment-wad)
  (;; This slot contains the number of consecutive initial semicolons
   ;; of the comment.
   (%semicolon-count :initarg :semicolon-count
                     :reader  semicolon-count)))

(defclass ignored-wad (skipped-wad)
  ())

;;; This class is used as a superclass for wad classes that represent reader
;;; conditionals, ones that skipped the guarded expression and ones that read
;;; the guarded expression.  What those have in common is the presence of a
;;; feature expression (unless the reader had to recover from invalid syntax in
;;; which case there may not be a feature expression).
(defclass conditional-wad ()
  (;; The raw feature expression, not the cst/wad.
   (%feature-expression :initarg :feature-expression
                        :reader  feature-expression)))

;;; Subclasses of this class are used to represent reader conditional for which
;;; the guarded expression was skipped.  Instances are not of type `cst:cst'.
;;; Usually there is a wad children for the feature expression (this child is
;;; usually of type `cst:cst') and a wad child for the skipped guarded
;;; expression (this child is usually of type `read-suppress-wad').  There may
;;; be other children in case material was skipped after reading the feature
;;; expression but before reading the guarded expression.
(defclass skipped-conditional-wad (maybe-extra-children-mixin
                                   conditional-wad
                                   ignored-wad)
  ())

(defclass skipped-positive-conditional-wad (skipped-conditional-wad) ())

(defclass skipped-negative-conditional-wad (skipped-conditional-wad) ())

;;; Subclasses of this class are used for reader conditionals for which the
;;; guarded expression was read.  Instances are of type `cst:cst'.  The target
;;; of the wrapper is the read guarded expression which is also of type
;;; `cst:cst'.  There is also a wad child which corresponds to the feature
;;; expression (this child is usually of type `cst:cst' as well).  There may be
;;; other children in case material was skipped after reading the feature
;;; expression but before reading the guarded expression.
(defclass read-conditional-wad (maybe-extra-children-mixin
                                conditional-wad
                                cst-wad
                                eclector.concrete-syntax-tree:wrapper-cst)
  ())

(defclass read-positive-conditional-wad (read-conditional-wad) ())

(defclass read-negative-conditional-wad (read-conditional-wad) ())

(defclass read-suppress-wad (maybe-extra-children-mixin ignored-wad)
  ())

(defclass reader-macro-wad (no-children-mixin ignored-wad)
  ())

;;; `text-wad'
;;;
;;; A wad that represents a piece of text such as word or punctuation
;;; within some atom (for example a symbol or a string) or skipped
;;; material (for example a semicolon comment or a block comment).  A
;;; `text-wad' does not have any children.

(defclass text-wad (no-children-mixin
                    non-cst-wad) ; TODO should not inherit indentation
  ())

(defclass punctuation-wad (text-wad)
  ())

(defclass word-wad (text-wad)
  ())

(defmethod errors ((wad word-wad))
  '())

(defmethod misspelled ((word-wad word-wad))
  (not (null (%errors word-wad))))

;;; `error-wad'
;;;
;;; Not a real wad but has the same location (start line/column,
;;; height, end column) information slots.

(defclass error-wad (basic-wad)
  ((%condition :initarg :condition
               :reader  condition)))

(defmethod print-object ((object error-wad) stream)
  (print-unreadable-object (object stream :type t)
    (print-wad-position object stream)
    (format stream " condition: ~a"
            (class-name (class-of (condition object))))))

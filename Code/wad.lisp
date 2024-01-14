(cl:in-package #:incrementalist)

;;; A WAD is the result of parsing an expression or some material that
;;; is normally skipped, such as a comment or an inactive reader
;;; conditional.

;;; During parsing, this variable holds the cache to which all created
;;; wads belong.
(defvar *cache*)

(defclass basic-wad ()
  (;; This slot contains the cache that this wad is part of.
   (%cache :initform *cache* :reader cache)
   ;; This slot contains the parent wad of this wad, or NIL if this
   ;; wad is a top-level wad.
   (%parent :initform nil :initarg :parent :accessor parent)
   ;; This slot contains the left sibling wad of this wad, or NIL if
   ;; this wad is the first child of its parent.  If this wad is a
   ;; top-level wad, then this slot contains the preceding top-level
   ;; wad, or NIL if this is the first top-level wad in the buffer.
   (%left-sibling
    :initform nil
    :initarg :left-sibling
    :accessor left-sibling)
   ;; This slot contains the right sibling wad of this wad, or NIL if
   ;; this wad is the last child of its parent.  If this wad is a
   ;; top-level wad, then this slot contains the following top-level
   ;; wad, or NIL if this is the last top-level wad in the buffer.
   (%right-sibling
    :initform nil
    :initarg :right-sibling
    :accessor right-sibling)
   ;; This slot contains the absolute start line of the wad.  Its
   ;; contents is valid only when the wad is on the prefix, and when
   ;; the wad is the top-level wad which is the first on the suffix.
   ;; With a wad in a different place, this slot may contain some
   ;; obsolete value.  We define a :BEFORE method on the slot reader
   ;; so that the wad of the argument will always be on the prefix
   ;; when the absolute line number is asked for.
   (%absolute-start-line-number
    :initarg :absolute-start-line-number
    :accessor absolute-start-line-number)
   ;; This slot contains information about the start line of the wad.
   ;; Simple applications might always store the absolute line number
   ;; of the first line of the wad in this slot.  Other applications
   ;; might store a line number relative to some other wad.
   (%start-line :initarg :start-line :accessor start-line)
   ;; This slot contains the difference between the start line and the
   ;; end line.  A value of 0 indicates that the wad starts and ends
   ;; in the same line.
   (%height :initarg :height :reader height)
   ;; This slot contains the absolute column of the first character in
   ;; this wad.  A value of 0 indicates that this wad starts in the
   ;; leftmost position in the source code.
   (%start-column :initarg :start-column :accessor start-column)
   ;; This slot contains the absolute column of the last character of
   ;; the wad.  The value of this slot can never be 0.  If the last
   ;; character of the wad is the leftmost character in a line, then
   ;; this slot contains the value 1.
   (%end-column :initarg :end-column :accessor end-column)
   ;; This slot contains the absolute column that the first character
   ;; of this wad should be positioned in, as computed by the rules of
   ;; indentation.  If this wad is not the first one on the line, then
   ;; this slot contains NIL.
   (%indentation :initform nil :initarg :indentation :accessor indentation)))

(defclass wad (basic-wad)
  (;; This slot contains the column number of the leftmost known
   ;; non-whitespace character of the wad.  It may not be entirely
   ;; correct if a reader macro reads character by character and such
   ;; characters happen to be outside the part that is returned by a
   ;; call to READ.  But we use this information only for
   ;; highlighting, and selection.  Not for drawing.
   (%min-column-number :initarg :min-column-number :reader min-column-number)
   ;; This slot contains the column number of the rightmost known
   ;; non-whitespace character of the wad.  It may not be entirely
   ;; correct for the same reason as the preceding slot.
   (%max-column-number :initarg :max-column-number :reader max-column-number)
   ;; This slot contains the maximum line width of any line that is
   ;; part of the wad.
   (%max-line-width :initarg :max-line-width :reader max-line-width)
   ;; This slot contains TRUE if and only if the START-LINE slot is
   ;; relative to some other line.
   (%relative-p :initarg :relative-p :accessor relative-p)
   (%children :initform '() :initarg :children :accessor children)))

(defun set-family-relations-of-children (wad)
  (let* ((children (children wad))
         (length (length children)))
    (loop for child in children
          do (setf (parent child) wad))
    (when (plusp length)
      (setf (left-sibling (first children)) nil)
      (setf (right-sibling (first (last children))) nil)
      (loop for (left right) on children
            repeat (1- length)
            do (setf (right-sibling left) right)
               (setf (left-sibling right) left)))))

(defmethod (setf children) :after (children (wad wad))
  (declare (ignorable children))
  (set-family-relations-of-children wad))

(defmethod shared-initialize :after ((wad wad) slot-names &key)
  (set-family-relations-of-children wad))

(defmethod initialize-instance :after ((object wad) &key)
  (let ((min-column-number (min (start-column object)
                                (end-column object)
                                (reduce #'min (children object)
                                        :initial-value 0
                                        :key #'min-column-number)))
        (max-column-number (max (start-column object)
                                (end-column object)
                                (reduce #'max (children object)
                                        :initial-value 0
                                        :key #'max-column-number))))
    (reinitialize-instance object
                           :min-column-number min-column-number
                           :max-column-number max-column-number)))

(defun print-wad-position (wad stream)
  (format stream "~:[abs~;rel~]:~d,~d -> ~d,~d"
          (relative-p wad)
          (start-line wad)
          (start-column wad)
          (if (relative-p wad)
              (height wad)
              (end-line wad))
          (end-column wad)))

(defmethod print-object ((object wad) stream)
  (print-unreadable-object (object stream :type t)
    (print-wad-position object stream)))

;;; Define an indirection for MAKE-INSTANCE for creating wads.  The
;;; main purpose is so that the creation of wads can be traced.
(defun make-wad (class &rest initargs)
  (apply #'make-instance class initargs))

(defclass expression-wad (wad)
  ((%expression :initarg :expression :accessor expression)))

(defmethod print-object ((object expression-wad) stream)
  (print-unreadable-object (object stream :type t)
    (print-wad-position object stream)
    (format stream " expression: ~S"
            (class-name (class-of (expression object))))))

(defclass labeled-object-definition-wad (expression-wad)
  ())

(defclass labeled-object-reference-wad (expression-wad)
  ())

(defclass no-expression-wad (wad)
  ())

(defclass skipped-wad (no-expression-wad)
  ())

;;; This class is the base class of all comment wads.
(defclass comment-wad (skipped-wad)
  ())

;;; This class is used for a block comment introduced by #|.
(defclass block-comment-wad (comment-wad)
  ())

;;; This class is used for a comment introduced by one or more
;;; semicolons.
(defclass semicolon-comment-wad (comment-wad)
  (;; This slot contains the number of consecutive initial semicolons
   ;; of the comment.
   (%semicolon-count :initarg :semicolon-count :reader semicolon-count)))

(defclass word-wad (skipped-wad)
  ((%misspelled :initarg :misspelled
                :reader  misspelled)))

(defclass ignored-wad (skipped-wad)
  ())

(defclass sharpsign-wad (ignored-wad)
  ((%expression :initarg :expression :reader expression)))

(defclass sharpsign-plus-wad (sharpsign-wad)
  ())

(defclass sharpsign-minus-wad (sharpsign-wad)
  ())

(defclass read-suppress-wad (ignored-wad)
  ())

(defclass reader-macro-wad (ignored-wad)
  ())

(defclass error-wad (wad)
  ((%condition :initarg :condition
               :reader  condition*)))

(defmethod print-object ((object error-wad) stream)
  (print-unreadable-object (object stream :type t)
    (print-wad-position object stream)
    (format stream " condition: ~a"
            (class-name (class-of (condition* object))))))

(defgeneric relative-to-absolute (wad offset)
  (:method ((p wad) offset)
    (assert (relative-p p))
    (incf (start-line p) offset)
    (setf (relative-p p) nil)))

(defgeneric absolute-to-relative (wad offset)
  (:method ((p wad) offset)
    (assert (not (relative-p p)))
    (decf (start-line p) offset)
    (setf (relative-p p) t)))

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
  (loop with base = offset
        for wad in absolute-wads
        for start-line = (start-line wad)
        do (absolute-to-relative wad base)
           (setf base start-line))
  absolute-wads)

(defgeneric end-line (wad)
  (:method ((p wad))
    (assert (not (relative-p p)))
    (+ (start-line p) (height p))))

(defun compute-absolute-line-numbers (top-level-wad)
  ;; Make sure the wad itself is absolute, so that we need to compute
  ;; the absolute line numbers only of its children.
  (assert (not (relative-p top-level-wad)))
  (setf (absolute-start-line-number top-level-wad)
        (start-line top-level-wad))
  (labels ((compute-line-numbers (relative-wads offset)
             (loop with base = offset
                   for wad in relative-wads
                   for absolute-start-line-number = (+ base (start-line wad))
                   do (setf (absolute-start-line-number wad)
                            absolute-start-line-number)
                      (setf base absolute-start-line-number)
                      (compute-line-numbers
                       (children wad) absolute-start-line-number))))
    (compute-line-numbers
     (children top-level-wad) (start-line top-level-wad))))

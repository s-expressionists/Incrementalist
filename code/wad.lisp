(cl:in-package #:incrementalist)

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
                         :type     (integer 0)
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

(defclass wad (family-relations-mixin basic-wad)
  (;; This slot contains the maximum line width of any line that is
   ;; part of the wad.
   (%max-line-width :initarg  :max-line-width
                    :reader   max-line-width)
   (%children       :initarg  :children
                    :reader   children
                    :initform '())
   ;; This slot contains the absolute column that the first character
   ;; of this wad should be positioned in, as computed by the rules of
   ;; indentation.  If this wad is not the first one on the line, then
   ;; this slot contains NIL.
   (%indentation    :initarg  :indentation
                    :accessor indentation
                    :initform nil)))

(declaim (inline link-siblings))
(defun link-siblings (left right)
  (unless (null left)
    (setf (right-sibling left) right))
  (unless (null right)
    (setf (left-sibling right) left)))

(defun make-children-relative-and-set-family-relations (wad)
  (assert (not (relative-p wad)))
  (let ((previous nil)
        (base     (start-line wad)))
    (flet ((adjust-child (child)
             ;; Make relative
             (setf base (absolute-to-relative child base))
             ;; Set relations
             (let ((parent (parent child)))
               (if (null parent)
                   (setf (parent child) wad)
                   (assert (eq parent wad)))
               (link-siblings previous child)
               (setf previous child))))
      (declare (dynamic-extent #'adjust-child))
      (mapc #'adjust-child (children wad))
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
                        (assert (relative-p child))
                        (let* ((start-line          (start-line child))
                               (absolute-start-line (+ base start-line)))
                          (declare (type alexandria:array-index
                                         start-line absolute-start-line))
                          (setf (absolute-start-line child)
                                absolute-start-line)
                          (process-wad child absolute-start-line)
                          (setf base absolute-start-line))))
                 (declare (dynamic-extent #'process-child))
                 (mapc #'process-child (children parent)))))
           (process-wad (wad wad-start-line)
             (process-children wad wad-start-line)))
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
(defun position-is-inside-wad-p (wad relative-line-number column-number)
  (not (or (position-is-before-wad-p wad relative-line-number column-number)
           (position-is-after-wad-p wad relative-line-number column-number))))

(defun wad-starts-before-wad-p (wad1 wad2)
  (%position< (start-line wad1) (start-column wad1)
              (start-line wad2) (start-column wad2)))

(defun wad-ends-after-wad-p (wad1 wad2)
  (not (%position< (end-line wad2) (end-column wad2)
                   (end-line wad1) (end-column wad1))))

(defun wad-contains-wad-p (wad1 wad2)
  (and (wad-starts-before-wad-p wad1 wad2)
       (wad-ends-after-wad-p wad1 wad2)))

(defclass expression-wad (wad)
  ((%expression :initarg :expression :accessor expression)))

(defmethod print-object ((object expression-wad) stream)
  (print-unreadable-object (object stream :type t)
    (print-wad-position object stream)
    (format stream " expression: ~S" (expression object))))

(defclass labeled-object-definition-wad (expression-wad)
  ())

(defclass labeled-object-reference-wad (expression-wad)
  ())

;;; Non-expression wads

(defclass no-expression-wad (basic-wad)
  ())

(defclass skipped-wad (no-expression-wad
                       wad)
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
   (%semicolon-count :initarg :semicolon-count
                     :reader  semicolon-count)))

(defclass ignored-wad (skipped-wad)
  ())

(defclass sharpsign-wad (ignored-wad)
  ((%expression :initarg :expression
                :reader  expression)))

(defclass sharpsign-plus-wad (sharpsign-wad)
  ())

(defclass sharpsign-minus-wad (sharpsign-wad)
  ())

(defclass read-suppress-wad (ignored-wad)
  ())

(defclass reader-macro-wad (ignored-wad)
  ())

;;; `word-wad'
;;;
;;; A wad that represents a word within some atom (for example a
;;; symbol or a string) or skipped material (for example a semicolon
;;; comment or a block comment).  A `word-wad' does not have any
;;; children.

(defclass word-wad (no-expression-wad wad) ; TODO should not inherit errors, indentation
  ((%misspelled :initarg :misspelled
                :reader  misspelled)))

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

(cl:in-package #:incrementalist)

;;; A class for presenting a snapshot of the contents of a Cluffer
;;; buffer as a (character input) stream.
;;;
;;; The operations `cl:file-position' (reading and writing) are not
;;; supported at the moment.
;;;
;;; An instance of this class stores the item sequences of the lines
;;; of a Cluffer buffer in its `%lines' slot.  The other crucial piece
;;; of state is current location which stored in the `%line-number'
;;; and `%item-number' slots.  The remaining slots cache information
;;; that would be too costly to compute or retrieve in every stream
;;; operation.
(defclass buffer-stream (stream:fundamental-character-input-stream)
  ((%lines       :initarg  :lines
                 :reader   lines)
   ;; Current position
   (%line-number :type     (or null alexandria:array-index)
                 :accessor line-number
                 :initform nil)
   (%item-number :type     alexandria:array-index
                 :accessor item-number)
   ;; Cached line information
   (%line-count  :accessor %line-count)
   (%line        :accessor %line)
   (%item-count  :accessor %item-count))
  (:default-initargs
   :lines (alexandria:required-argument :lines)))

(declaim (inline update-lines-cache update-line-cache))
(defun update-lines-cache (stream lines)
  (setf (line-number stream) nil ; forces update of `%line', `%item-count'
        (%line-count stream) (flx:nb-elements lines)))

(defun update-line-cache (stream lines old-line-number new-line-number)
  (unless (eql new-line-number old-line-number)
    (let ((line (flx:element* lines new-line-number)))
      (setf (%line       stream) line
            (%item-count stream) (length line)))))

(defmethod shared-initialize :after ((instance   buffer-stream)
                                     (slot-names t)
                                     &key (lines nil lines-supplied-p))
  (when lines-supplied-p
    (update-lines-cache instance lines)))

(defmethod (setf line-number) :around ((new-value integer)
                                       (object    buffer-stream))
  (let ((old-value (line-number object)))
    (call-next-method)
    (update-line-cache object (lines object) old-value new-value)))

(defmethod print-object ((object buffer-stream) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~:[N/A~;~:*~A~],~:[N/A~;~:*~A~]"
            (line-number object)
            (if (slot-boundp object '%item-number)
                (item-number object)
                nil))))

;;; Gray stream protocol

(defmethod stream:stream-peek-char ((stream buffer-stream))
  (let* ((item-number   (item-number stream))
         (end-of-line-p (= item-number (the alexandria:array-index
                                            (%item-count stream)))))
    (declare (type alexandria:array-index item-number))
    (cond ((not end-of-line-p)
           (let ((line (%line stream)))
             (declare (type simple-string line))
             (aref line item-number)))
          ((= (the alexandria:array-index (line-number stream))
              (1- (the alexandria:array-index (%line-count stream))))
           :eof)
          (t
           #\Newline))))

(defmethod stream:stream-read-char ((stream buffer-stream))
  (let* (line-number
         (item-number   (item-number stream))
         (end-of-line-p (= item-number (the alexandria:array-index
                                            (%item-count stream)))))
    (declare (type alexandria:array-index item-number))
    (cond ((not end-of-line-p)
           (prog1
               (let ((line (%line stream)))
                 (declare (type simple-string line))
                 (aref line item-number))
             (setf (item-number stream) (1+ item-number))))
          ((= (setf line-number (the alexandria:array-index (line-number stream)))
              (1- (the alexandria:array-index (%line-count stream))))
           :eof)
          (t
           (prog1
               #\Newline
             (setf (line-number stream) (1+ line-number) ; updates cache
                   (item-number stream) 0))))))

(defmethod stream:stream-unread-char ((stream buffer-stream) (char t))
  (let* (line-number
         (item-number         (item-number stream))
         (beginning-of-line-p (zerop item-number)))
    (cond ((not beginning-of-line-p)
           (setf (item-number stream) (1- item-number)))
          ((zerop (setf line-number (line-number stream)))
           (error "Attempt to unread a character at position 0"))
          (t
           (setf (line-number stream) (1- line-number) ; updates cache
                 (item-number stream) (length (%line stream)))))))

(defun compute-max-line-width (buffer-stream start-line end-line children)
  (let ((lines (lines buffer-stream)))
    (loop with rest = children
          for line-number from start-line
          while (<= line-number end-line)
          if (and (not (null rest)) (= line-number (start-line (first rest))))
            maximize (max-line-width (first rest))
            and do (setf line-number (end-line (first rest)))
                   (pop rest)
          else
            maximize (length (flx:element* lines line-number)))))

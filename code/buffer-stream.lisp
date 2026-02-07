(cl:in-package #:incrementalist)

;;; TODO: text.editor-buffer should provide this; will that remove trivial-gray-stream dependency?

(deftype stream-line-designator ()
  '(or null stream-line-number))

(deftype stream-line ()
  'simple-string)

(deftype stream-line-number ()
  'alexandria:array-index)

(deftype stream-item-number ()
  'alexandria:array-index)

;;; A class for presenting a snapshot of the contents of a
;;; text.editor-buffer buffer as a (character input) stream.
;;;
;;; The operations `cl:file-position' (reading and writing) are not
;;; supported at the moment.
;;;
;;; An instance of this class stores the item sequences of the lines
;;; of a text.editor-buffer buffer in its `%lines' slot.  The other
;;; crucial piece of state is current location which stored in the
;;; `%line-number' and `%item-number' slots.  The remaining slots
;;; cache information that would be too costly to compute or retrieve
;;; in every stream operation.
(defclass buffer-stream (stream:fundamental-character-input-stream)
  ((%lines       :initarg  :lines
                 :reader   lines)
   ;; Current position
   (%line-number :type     stream-line-designator
                 :reader   line-number
                 :writer   (setf %line-number)
                 :initform nil)
   (%item-number :type     alexandria:array-index
                 :accessor item-number)
   ;; Cached line information
   (%line-count  :accessor %line-count)
   (%line        :type     stream-line
                 :accessor %line)
   (%item-count  :accessor %item-count))
  (:default-initargs
   :lines (alexandria:required-argument :lines)))

(declaim (inline update-lines-cache update-line-cache))
(defun update-lines-cache (stream lines)
  (setf (%line-number stream) nil
        (%line-count stream)  (flx:nb-elements lines)))

(defun update-line-cache (stream line-number)
  (let* ((lines (lines stream))
         (line  (flx:element* lines line-number)))
    (declare (type stream-line line))
    (setf (%line       stream) line
          (%item-count stream) (length line))))

(defmethod shared-initialize :after ((instance   buffer-stream)
                                     (slot-names t)
                                     &key (lines nil lines-supplied-p))
  (when lines-supplied-p
    (update-lines-cache instance lines)))

(defmethod (setf line-number) ((new-value t) (object buffer-stream))
  (let ((old-value (line-number object)))
    (declare (type stream-line-designator old-value))
    (unless (eql new-value old-value)
      (setf (%line-number object) new-value)
      (update-line-cache object new-value)))
  new-value)

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
         (end-of-line-p (= item-number (the stream-item-number
                                            (%item-count stream)))))
    (declare (type stream-item-number item-number))
    (cond ((not end-of-line-p)
           (let ((line (%line stream)))
             (declare (type stream-line line))
             (aref line item-number)))
          ((= (the stream-line-number (line-number stream))
              (1- (the stream-line-number (%line-count stream))))
           :eof)
          (t
           #\Newline))))

(defmethod stream:stream-read-char ((stream buffer-stream))
  (let* (line-number
         (item-number   (item-number stream))
         (end-of-line-p (= item-number (the stream-item-number
                                            (%item-count stream)))))
    (declare (type (or null stream-line-number) line-number)
             (type stream-item-number           item-number))
    (cond ((not end-of-line-p)
           (prog1
               (let ((line (%line stream)))
                 (declare (type stream-line line))
                 (aref line item-number))
             (setf (item-number stream) (1+ item-number))))
          ((= (setf line-number (line-number stream))
              (1- (the stream-line-number (%line-count stream))))
           :eof)
          (t
           (prog1
               #\Newline
             (let ((new-line-number (1+ line-number)))
               (setf (%line-number stream) new-line-number)
               (update-line-cache stream new-line-number)
               (setf (item-number stream) 0)))))))

(defmethod stream:stream-unread-char ((stream buffer-stream) (char t))
  (let* (line-number
         (item-number         (item-number stream))
         (beginning-of-line-p (zerop item-number)))
    (declare (type (or null stream-line-number) line-number)
             (type stream-item-number           item-number))
    (cond ((not beginning-of-line-p)
           (setf (item-number stream) (1- item-number)))
          ((zerop (setf line-number (line-number stream)))
           (error "Attempt to unread a character at position 0"))
          (t
           (let ((new-line-number (1- line-number)))
             (setf (%line-number stream) new-line-number)
             (update-line-cache stream new-line-number)
             (setf (item-number stream) (%item-count stream)))))))

(defun compute-max-line-width (buffer-stream start-line end-line children)
  (loop :with lines = (lines buffer-stream)
        :with rest  = children
        :for line-number :of-type stream-line-number :from start-line
        :while (<= line-number end-line)
        :if (and (not (null rest))
                 (= line-number
                    (the stream-line-number (start-line (first rest)))))
          :maximize (the alexandria:array-index (max-line-width (first rest)))
          :and :do (setf line-number (end-line (first rest)))
                   (pop rest)
        :else
          :maximize (length (the stream-line (flx:element* lines line-number)))))

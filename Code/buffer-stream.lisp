(cl:in-package #:incrementalist)

(defgeneric eof-p (buffer-stream))

(defgeneric next-position (lines line-number item-number))

(defgeneric previous-position (lines line-number item-number))

(defgeneric forward (buffer-stream))

(defgeneric backward (buffer-stream))

(defclass buffer-stream (gs:fundamental-character-input-stream)
  ((%lines :initarg :lines :reader lines)
   (%current-line-number :initform 0 :accessor current-line-number)
   (%current-item-number :initform 0 :accessor current-item-number)))

(defmethod eof-p ((stream buffer-stream))
  (let* ((lines (lines stream))
         (last-line-number (1- (flx:nb-elements lines)))
         (last-line (flx:element* lines last-line-number))
         (last-line-length (length last-line)))
    (and (= (current-line-number stream) last-line-number)
         (= (current-item-number stream) last-line-length))))

(defmethod next-position ((lines flx:flexichain) line-number item-number)
  (if (= (length (flx:element* lines line-number)) item-number)
      (values (1+ line-number) 0)
      (values line-number (1+ item-number))))

(defmethod previous-position ((lines flx:flexichain) line-number item-number)
  (if (zerop item-number)
      (values (1- line-number)
              (length (flx:element* lines (1- line-number))))
      (values line-number (1- item-number))))

(macrolet ((define (name position-method)
             `(defmethod ,name ((stream buffer-stream))
                (with-accessors ((lines lines)
                                 (current-line-number current-line-number)
                                 (current-item-number current-item-number))
                    stream
                  (setf (values current-line-number current-item-number)
                        (,position-method lines current-line-number current-item-number))))))
  (define forward  next-position)
  (define backward previous-position))

(defmethod gs:stream-peek-char ((stream buffer-stream))
  (if (eof-p stream)
      :eof
      (let* ((lines (lines stream))
             (current-line-number (current-line-number stream))
             (current-item-number (current-item-number stream))
             (line (flx:element* lines current-line-number)))
        (if (= (length line) current-item-number)
            #\Newline
            (aref line current-item-number)))))

(defmethod gs:stream-read-char ((stream buffer-stream))
  (let ((result (gs:stream-peek-char stream)))
    (prog1
        result
      (unless (eq result :eof)
        (forward stream)))))

(defmethod gs:stream-unread-char ((stream buffer-stream) char)
  (declare (ignore char))
  (backward stream))

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

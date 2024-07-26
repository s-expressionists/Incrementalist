(cl:in-package #:incrementalist)

(defclass analyzer (buffer-stream)
  ((%buffer :initarg :buffer
            :reader  buffer)
   (%cache  :initarg :cache
            :reader  cache))
  (:default-initargs
   :buffer (alexandria:required-argument :buffer)
   :lines  nil))

(defmethod shared-initialize :around ((instance   analyzer)
                                      (slot-names t)
                                      &rest args
                                      &key (buffer nil buffer-supplied-p)
                                           (cache  nil cache-supplied-p))
  (cond ((not buffer-supplied-p)
         (call-next-method))
        (cache-supplied-p
         (let ((lines (lines cache)))
           (apply #'call-next-method instance slot-names :lines lines args)))
        (t
         (let* ((cache (make-instance 'cache :buffer buffer))
                (lines (lines cache)))
           (apply #'call-next-method instance slot-names
                  :cache cache :lines lines args)))))

(defmethod position< ((left basic-wad) (right analyzer))
  (%position< (start-line left)   (start-column left)
              (line-number right) (item-number right)))

(defmethod position> ((left basic-wad) (right analyzer))
  (%position> (start-line left)   (start-column left)
              (line-number right) (item-number right)))

(defmethod position= ((left basic-wad) (right analyzer))
  (and (= (start-line left)   (line-number right))
       (= (start-column left) (item-number right))))

(defun advance-stream-to-beyond-wad (analyzer wad)
  (setf (line-number analyzer) (end-line wad)
        (item-number analyzer) (end-column wad)))

;;; Check whether there is a cached wad with a start position that
;;; corresponds to the current stream position of ANALYZER, and if so,
;;; return that wad.  If there is no such parse result, then return
;;; NIL.  If there are cached wads that entirely precede the current
;;; stream position, then remove them.
;;;
;;; If we return a wad at all, it has the following properties:
;;; 1. it is absolute
;;  2. all its descendants are relative
;;; 3. the absolute line start line number are up-to-date for the
;;;    returned wad and all its descendants
(defun cached-wad (analyzer)
  (let ((cache (cache analyzer)))
    (macrolet ((skip (reader popper)
                 `(progn
                    (loop for maybe-wad = (,reader cache)
                          while (and (not (null maybe-wad))
                                     (position< (first maybe-wad) analyzer))
                          do (,popper cache))
                    (,reader cache))))
      (let ((residue (skip residue pop-from-residue)))
        (cond ((null residue)
               (let ((suffix (skip suffix pop-from-suffix)))
                 (if (and (not (null suffix))
                          (position= (first suffix) analyzer))
                     (compute-absolute-line-numbers (pop-from-suffix cache))
                     nil)))
              ((position= (first residue) analyzer)
               (compute-absolute-line-numbers (first residue)))
              (t
               nil))))))

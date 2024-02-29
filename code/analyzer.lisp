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
         (let* ((cache (make-instance 'cache :cluffer-buffer buffer))
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

;;; Check whether there is a cached wad with a start position that
;;; corresponds to the current stream position of ANALYZER, and if so,
;;; return that wad.  If there is no such parse result, then return
;;; NIL.  If there are cached wads that entirely precede the current
;;; stream position, then remove them.
(defun cached-wad (analyzer)
  (let ((cache (cache analyzer)))
    (with-accessors ((residue residue) (suffix suffix)) cache
      (loop while (and (not (null residue))
                       (position< (first residue) analyzer))
            do (pop-from-residue cache))
      (if (not (null residue))
          (if (position= (first residue) analyzer)
              (first residue)
              nil)
          (progn
            (loop while (and (not (null suffix))
                             (position< (first suffix) analyzer))
                  do (pop-from-suffix cache))
            (if (not (null suffix))
                (if (position= (first suffix) analyzer)
                    (first suffix)
                    nil)
                nil))))))

(defun advance-stream-to-beyond-wad (analyzer wad)
  (setf (line-number analyzer) (end-line wad)
        (item-number analyzer) (end-column wad)))

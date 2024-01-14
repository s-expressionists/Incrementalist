(cl:in-package #:incrementalist)

(defclass analyzer (buffer-stream)
  ((%buffer :initarg :buffer :reader buffer)
   (%cache :initarg :cache :reader cache)))

(defmethod print-object ((object analyzer) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A,~A"
            (current-line-number object) (current-item-number object))))

(defmethod position< ((left basic-wad) (right analyzer))
  (%position< (start-line left)           (start-column left)
              (current-line-number right) (current-item-number right)))

(defmethod position> ((left basic-wad) (right analyzer))
  (%position> (start-line left)           (start-column left)
              (current-line-number right) (current-item-number right)))

(defmethod position= ((left basic-wad) (right analyzer))
  (and (= (start-line left)   (current-line-number right))
       (= (start-column left) (current-item-number right))))

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
  (setf (current-line-number analyzer)
        (end-line wad))
  (setf (current-item-number analyzer)
        (end-column wad)))

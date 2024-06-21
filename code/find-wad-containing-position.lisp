(cl:in-package #:incrementalist)

;;; Helper function.
(defun traverse-relative-wads
    (wads line-number column-number reference-line-number
     &key (start-relation '<=) (end-relation '<))
  (loop for wad in wads
        for relative-line-number
           = (- line-number reference-line-number)
        until (%position< relative-line-number column-number
                          (start-line wad) (start-column wad))
        when (position-is-inside-wad-p
              wad relative-line-number column-number
              :start-relation start-relation :end-relation end-relation)
          return (values wad (+ reference-line-number (start-line wad)))
        do (incf reference-line-number (start-line wad))))

;;; Return a top-level wad in the prefix of CACHE that contains the
;;; position indicated by LINE-NUMBER and COLUMN-NUMBER.  If no wad in
;;; the prefix contains the position, then return NIL.
(defun find-wad-containing-position-in-prefix
    (cache line-number column-number
     &key (start-relation '<=) (end-relation '<))
  (loop for wad in (prefix cache)
        until (%position> line-number column-number
                          (+ (start-line wad) (height wad)) (end-column wad))
        when (position-is-inside-wad-p
              wad line-number column-number
              :start-relation start-relation :end-relation end-relation)
          return wad))

;;; Return a top-level wad in the suffix of CACHE that contains the
;;; position indicated by LINE-NUMBER and COLUMN-NUMBER.  As a second
;;; return value, return the absolute line number of the start line of
;;; the wad that was found.  If no wad in the prefix contains the
;;; position, then return NIL.
(defun find-wad-containing-position-in-suffix (cache line-number column-number
                                               &key (start-relation '<=)
                                                    (end-relation   '<))
  (let ((suffix (suffix cache)))
    (cond ((null suffix)
           nil)
          ((position-is-inside-wad-p
            (first suffix) line-number column-number
            :start-relation start-relation :end-relation end-relation)
           (let ((first (first suffix)))
             (values first (start-line first))))
          (t
           (destructuring-bind (first &rest rest) suffix
             (traverse-relative-wads
              rest line-number column-number (start-line first)
              :start-relation start-relation :end-relation end-relation))))))

(defmethod find-wads-containing-position ((cache         cache)
                                          (line-number   integer)
                                          (column-number integer))
  (let ((result '()))
    (labels ((traverse-children (children reference-line-number)
               (multiple-value-bind (wad absolute-start-line)
                   (traverse-relative-wads
                    children line-number column-number reference-line-number)
                 (if (null wad)
                     (return-from find-wads-containing-position result)
                     (progn (push (cons absolute-start-line wad) result)
                            (traverse-children
                             (children wad) absolute-start-line))))))
      (let ((wad (find-wad-containing-position-in-prefix
                  cache line-number column-number)))
        (if (null wad)
            (multiple-value-bind (wad absolute-line-number)
                (find-wad-containing-position-in-suffix
                 cache line-number column-number)
              (if (null wad)
                  nil
                  (progn
                    (push (cons absolute-line-number wad) result)
                    (traverse-children (children wad) absolute-line-number))))
            (progn
              (push (cons (start-line wad) wad) result)
              (traverse-children (children wad) (start-line wad))))))))

(defmethod map-wads-containing-position ((function      t)
                                         (cache         cache)
                                         (line-number   integer)
                                         (column-number integer)
                                         &key (start-relation '<=)
                                              (end-relation   '<))
  (let ((function (alexandria:ensure-function function)))
    (labels ((traverse-children (reference-line-number parent)
               (multiple-value-bind (wad absolute-start-line)
                   (traverse-relative-wads
                    (children parent)
                    line-number column-number reference-line-number
                    :start-relation start-relation :end-relation end-relation)
                 (when (not (null wad))
                   (traverse-children absolute-start-line wad)
                   (funcall function absolute-start-line wad)))))
      (let ((wad (find-wad-containing-position-in-prefix
                  cache line-number column-number :start-relation start-relation
                                                  :end-relation   end-relation)))
        (if (null wad)
            (multiple-value-bind (wad absolute-line-number)
                (find-wad-containing-position-in-suffix
                 cache line-number column-number :start-relation start-relation
                                                 :end-relation   end-relation)
              (unless (null wad)
                (traverse-children absolute-line-number wad)
                (funcall function absolute-line-number wad)))
            (let ((start-line (start-line wad)))
              (traverse-children start-line wad)
              (funcall function start-line wad)))))))

(cl:in-package #:incrementalist)

;;; Return a top-level wad in the prefix of CACHE that contains the
;;; position indicated by LINE-NUMBER and COLUMN-NUMBER.  If no wad in
;;; the prefix contains the position, then return NIL.
(defun find-wad-containing-position-in-prefix (cache line-number column-number)
  (loop for wad in (prefix cache)
        until (position-is-after-wad-p wad line-number column-number)
        when (position-is-inside-wad-p wad line-number column-number)
          return wad))

;;; Helper function.
(defun traverse-relative-wads
    (wads line-number column-number reference-line-number)
  (loop for wad in wads
        for relative-line-number
          = (- line-number reference-line-number)
        until (position-is-before-wad-p
               wad relative-line-number column-number)
        when (position-is-inside-wad-p
              wad relative-line-number column-number)
          return (values wad (+ reference-line-number (start-line wad)))
        do (incf reference-line-number (start-line wad))))

;;; Return a top-level wad in the suffix of CACHE that contains the
;;; position indicated by LINE-NUMBER and COLUMN-NUMBER.  As a second
;;; return value, return the absolute line number of the start line of
;;; the wad that was found.  If no wad in the prefix contains the
;;; position, then return NIL.
(defun find-wad-containing-position-in-suffix (cache line-number column-number)
  (let ((suffix (suffix cache)))
    (cond ((null suffix)
           nil)
          ((position-is-inside-wad-p (first suffix) line-number column-number)
           (values (first suffix) (start-line (first suffix))))
          (t
           (traverse-relative-wads
            (rest suffix)
            line-number
            column-number
            (start-line (first suffix)))))))

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

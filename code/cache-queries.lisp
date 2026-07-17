(cl:in-package #:incrementalist)

;;; Given a line number, This function returns a wad that starts on a
;;; particular line number, such that no other wad starts to the left
;;; of it on the same line.  If no wad can be found that fulfills this
;;; requirement, then NIL is returned.
;;;
;;; This function probably doesn't have to be particularly efficient,
;;; so we do the simplest possible thing, namely we move all wads to
;;; the prefix (which the line numbers are absolute), and then we scan
;;; the prefix from the beginning.
(defun search-wad (wad start-line-number line-number)
  (if (= start-line-number line-number)
      wad
      (loop for child in (children wad)
            for absolute-line-number = start-line-number
              then (+ absolute-line-number (start-line child))
            when (<= absolute-line-number
                     line-number
                     (+ absolute-line-number (height child)))
              return (search-wad child absolute-line-number line-number))))

(defmethod find-wad-beginning-line ((cache cache) (line-number integer))
  (repeat-suffix-to-prefix cache)
  (loop for wad in (reverse (prefix cache))
        for start-line = (start-line wad)
        when (<= start-line line-number (+ start-line (height wad)))
          return (search-wad wad start-line line-number)))

;;;; Functions for mapping over wads at a specific location

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
(defun find-wad-containing-position-in-prefix (cache line-number column-number
                                               &key (start-relation '<=)
                                                    (end-relation   '<))
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

(defmethod find-wads-containing-position ((cache         cache)
                                          (line-number   integer)
                                          (column-number integer)
                                          &key (start-relation '<=)
                                               (end-relation   '<))
  (let ((result '()))
    (map-wads-containing-position
     (lambda (start-line wad)
       (push (cons start-line wad) result))
     cache line-number column-number :start-relation start-relation
                                     :end-relation   end-relation)
    (nreverse result)))

;;;; Functions for mapping over cache regions

(defmethod map-wads ((function t) (cache cache) &key (start-line 0)
                                                     (end-line nil))
  ;; Make sure no wad on the suffix starts at or before END-LINE.
  (repeat-suffix-to-prefix cache end-line)
  ;; Find a suffix of the prefix (i.e., a prefix of wads in the
  ;; buffer) such that it is not the case that the first wad of the
  ;; prefix (i.e., the last wad of the buffer prefix) starts entirely
  ;; after END-LINE.
  (let ((remaining (loop :for remaining :on (prefix cache)
                         :when (or (null end-line)
                                   (<= (start-line (first remaining)) end-line))
                           :return remaining)))
    (unless (null remaining)
      (loop :with function = (alexandria:ensure-function function)
            :for wad :in remaining
            :until (or (null wad) (< (end-line wad) start-line))
            :do (funcall function wad)))))

(defmethod map-empty-area ((function t) (cache cache)
                           &key (start-line   0)
                                (start-column 0)
                                (end-line     (1- (line-count cache)))
                                (end-column   (line-length cache end-line)))
  (when (%position< start-line start-column end-line end-column)
    (loop :with function          = (alexandria:ensure-function function)
          :for  line-number       :from end-line :downto start-line
          :for  line-length       =     (line-length cache line-number)
          :for  line-start-column =     (if (= line-number start-line)
                                            start-column
                                            0)
          :for  line-end-column   =     (if (= line-number end-line)
                                            end-column
                                            line-length)
          :when (< line-start-column line-end-column)
            :do (funcall function line-number line-start-column line-end-column))))

(defmethod map-wads-and-spaces ((wad-function   t)
                                (space-function t)
                                (cache          cache)
                                &key (start-line 0)
                                     (end-line   nil))
  ;; Make sure no wad on the suffix starts at or before END-LINE.
  (repeat-suffix-to-prefix cache end-line)
  ;; Find a suffix of the prefix (i.e., a prefix of wads in the
  ;; buffer) such that it is not the case that the first wad of the
  ;; prefix (i.e., the last wad of the buffer prefix) starts entirely
  ;; after END-LINE.
  (let ((remaining (loop :for remaining :on (prefix cache)
                         :when (or (null end-line)
                                   (<= (start-line (first remaining)) end-line))
                           :return remaining))
        (end-line  (if (null end-line)
                       (1- (line-count cache))
                       end-line)))
    (if (null remaining)
        (map-empty-area space-function cache :start-line start-line
                                             :end-line   end-line)
        (let* ((first              (first remaining))
               (first-wad-end-line (end-line first)))
          ;; Cover the empty area (if any) between the END-LINE and
          ;; the end of the first (in reverse order of the prefix)
          ;; wad.  If this first wad ends before START-LINE, clamp the
          ;; empty area to START-LINE.
          (when (<= first-wad-end-line end-line)
            (multiple-value-bind (start-line start-column)
                (if (< first-wad-end-line start-line)
                    (values start-line         0)
                    (values first-wad-end-line (end-column first)))
              (map-empty-area space-function cache :start-line   start-line
                                                   :start-column start-column
                                                   :end-line     end-line)))
          (loop :with wad-function      =   (alexandria:ensure-function
                                             wad-function)
                :for  (wad2 wad1)       :on remaining
                :for  wad-end-line      =   first-wad-end-line
                  :then next-wad-end-line
                :for  next-wad-end-line =   (unless (null wad1) (end-line wad1))
                :until (or (null wad2) (< wad-end-line start-line))
                :do (funcall wad-function wad2)
                    (multiple-value-bind (start-line start-column)
                        (if (or (null wad1) (< next-wad-end-line start-line))
                            (values start-line        0)
                            (values next-wad-end-line (end-column wad1)))
                      (map-empty-area space-function cache
                                      :start-line   start-line
                                      :start-column start-column
                                      :end-line     (start-line wad2)
                                      :end-column   (start-column wad2))))))))

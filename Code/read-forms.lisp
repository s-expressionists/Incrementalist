(cl:in-package #:incrementalist)

(defun read-forms (analyzer)
  (with-accessors ((cache cache)
                   (current-line-number current-line-number)
                   (current-item-number current-item-number))
      analyzer
    (with-accessors ((prefix prefix) (suffix suffix) (residue residue))
        cache
      ;; Position stream after last prefix wad (remember the cache
      ;; prefix is stored in reverse order) if any.
      (setf (values current-line-number current-item-number)
            (if (null prefix)
                (values 0 0)
                (let ((first (first prefix)))
                  (values (end-line first) (end-column first)))))
      (let ((client (make-instance 'client :stream* analyzer))
            (*cache* cache))
        (loop for kind = (eclector.reader:call-as-top-level-read
                          client (lambda ()
                                   (parse-and-cache analyzer client))
                          analyzer t nil nil)
              when (or (eq kind :eof)
                       (and (not (null suffix))
                            (position= (first suffix) analyzer)))
                ;; If we reach EOF while reading whitespace, suffix
                ;; must be empty, and the residue is either empty or
                ;; it contains wads that should be removed.  If we do
                ;; not reach EOF, then we stop only if the current
                ;; position is that of the first parse result on the
                ;; suffix.
                do (setf residue '())
                   (return-from read-forms nil)
              ;; In case we skipped some whitespace, discard any wads
              ;; on the cache residue and cache suffix that are now
              ;; before the current stream position.
              unless (eq kind :whitespace)
                do (loop while (and (not (null residue))
                                    (position< (first residue) analyzer))
                         do (pop-from-residue cache))
                   (when (null residue)
                     (loop while (and (not (null suffix))
                                      (position< (first suffix) analyzer))
                           do (pop-from-suffix cache))))))))

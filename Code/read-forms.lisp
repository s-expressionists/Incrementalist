(cl:in-package #:incrementalist)

(defun read-forms (analyzer)
  (with-accessors ((cache cache)
                   (current-line-number current-line-number)
                   (current-item-number current-item-number))
      analyzer
    (with-accessors ((prefix prefix) (suffix suffix) (residue residue))
        cache
      (if (null prefix)
          (setf current-line-number 0
                current-item-number 0)
          (setf current-line-number (end-line (first prefix))
                current-item-number (end-column (first prefix))))
      (let ((client (make-instance 'client :stream* analyzer))
            (*cache* cache))
        (eclector.reader:call-as-top-level-read
         client
         (lambda ()
           (loop do (skip-whitespace analyzer)
                    (when (or (eof-p analyzer)
                              (and (not (null suffix))
                                   (position= (first suffix) analyzer)))
                      ;; If we reach EOF while reading whitespace,
                      ;; suffix must be empty, and the residue is
                      ;; either empty or it contains wads that should
                      ;; be removed.  If we do not reach EOF, then we
                      ;; stop only if the current position is that of
                      ;; the first parse result on the suffix.
                      (setf residue '())
                      (return-from read-forms nil))
                    (parse-and-cache analyzer client)
                    (loop while (and (not (null residue))
                                     (position< (first residue) analyzer))
                          do (pop-from-residue cache))
                    (when (null residue)
                      (loop while (and (not (null suffix))
                                       (position< (first suffix) analyzer))
                            do (pop-from-suffix cache)))))
         analyzer t nil nil)))))

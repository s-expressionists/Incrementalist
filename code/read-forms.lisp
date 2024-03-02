(cl:in-package #:incrementalist)

(defun read-forms (analyzer)
  (let ((cache (cache analyzer)))
    ;; Position ANALYZER (which is a stream) after the last prefix wad
    ;; (remember the cache prefix is stored in reverse order) if any.
    (update-lines-cache analyzer (lines analyzer))
    (let ((prefix (prefix cache)))
      (setf (values (line-number analyzer) (item-number analyzer))
            (if (null prefix)
                (values 0 0)
                (let ((first (first prefix)))
                  (values (end-line first) (end-column first))))))
    ;; Keep reading top-level expressions until the stream position is
    ;; at the start of the cache suffix (wads on the cache suffix are
    ;; unmodified and relative except the first one, so neither
    ;; re-READing nor adjusting them is necessary) or the EOF is
    ;; reached.  Remove wads from the cache residue and cache suffix
    ;; as the stream positions passes them by.  Push newly created
    ;; wads to the cache prefix (this happens in `parse-and-cache').
    (loop with client  = (make-instance 'client :stream* analyzer)
          with *cache* = cache
          for kind = (eclector.reader:call-as-top-level-read
                      client (lambda ()
                               (parse-and-cache analyzer client))
                      analyzer t nil nil)
          ;; If we reach EOF while reading whitespace, the cache
          ;; suffix must be empty, and the cache residue is either
          ;; empty or it contains wads that should be removed.  If we
          ;; do not reach EOF, then we stop only if the current
          ;; position is that of the first parse result on the cache
          ;; suffix.
          when (or (eq kind :eof)
                   (let ((suffix (suffix cache)))
                     (and (not (null suffix))
                          (position= (first suffix) analyzer))))
            do (setf (residue cache) '())
               (return-from read-forms nil)
          ;; In case we skipped some whitespace, discard any wads on
          ;; the cache residue and cache suffix that are now before
          ;; the current stream position.
          unless (eq kind :whitespace)
            do (loop for residue = (residue cache)
                     while (and (not (null residue))
                                (position< (first residue) analyzer))
                     do (pop-from-residue cache))
               (when (null (residue cache))
                 (loop for suffix = (suffix cache)
                       while (and (not (null suffix))
                                  (position< (first suffix) analyzer))
                       do (pop-from-suffix cache))))))

(cl:in-package #:incrementalist)

(defun make-error-wad (condition start-line start-column end-line end-column)
  (let ((height (- end-line start-line)))
    (make-instance 'error-wad :cache               *cache*
                              :relative-p          nil
                              :absolute-start-line start-line
                              :start-line          start-line
                              :height              height
                              :start-column        start-column
                              :end-column          end-column
                              :condition           condition)))

;;; PARSE-AND-CACHE collects Eclector errors in *ERRORS* during
;;; parsing.
(defvar *errors*)

;;; Establish a handler which collects `reader-error's into `*errors*' and
;;; recovers around BODY.
(defmacro with-error-recording (() &body body)
  `(handler-bind
       ((reader-error
          (lambda (condition)
            (destructuring-bind (line . column)
                (eclector.base:stream-position condition)
              (let* ((offset       (eclector.base:position-offset condition))
                     (length       (eclector.base:range-length condition))
                     (start-column (max 0 (+ column offset)))
                     (end-column   (+ start-column length))
                     (error-wad    (make-error-wad
                                    condition
                                    line start-column line end-column)))
                (push error-wad *errors*)))
            (eclector.reader:recover))))
     ,@body))

(defmethod reader:read-maybe-nothing :around
    ((client client) (stream analyzer) eof-error-p eof-value)
  (let ((cached (cached-wad stream)))
    (if (or (null cached)
            ;; Can't use zero length cached wad (can happen for
            ;; `error-wad') since the `advance-stream-to-beyond-wad'
            ;; would not advance in that case and the read loop would
            ;; not make any progress.
            (and (zerop (height cached))
                 (= (start-column cached) (end-column cached))))
        ;; Nothing has been cached, so call
        ;; `read-maybe-nothing'. Collect errors in *ERRORS* and
        ;; integrate them into RESULT.
        (let ((*errors* '()))
          (multiple-value-bind (object kind result)
              (call-next-method)
            (when (and (not (null result)) ; RESULT can be `null' for `:skip'
                       (member kind '(:object :skip)))
              (add-errors result *errors*))
            (values object kind result)))
        ;; There is a cached wad for the current input position. Turn
        ;; the wad into appropriate return values, inject it into
        ;; Eclector's result stack and advance STREAM.
        (multiple-value-prog1
            (etypecase cached
              (read-suppress-wad (values nil              :suppress cached t))
              (non-cst-wad       (values nil              :skip     cached t))
              (cst-wad           (values (cst:raw cached) :object   cached t)))
          (push cached (first eclector.parse-result::*stack*)) ; HACK
          (advance-stream-to-beyond-wad stream cached)))))

(defun read-and-cache (analyzer client)
  ;; Use ECLECTOR.READER:READ-MAYBE-NOTHING to read either a single
  ;; skipped input or an object. Both are pushed into the prefix of
  ;; the cache that is associated with ANALYZER.
  ;;
  ;; For any errors during parsing, WITH-ERROR-RECORDING creates an
  ;; ERROR-WAD records it in *ERRORS*, then asks Eclector to perform
  ;; the appropriate recovery. The READ-MAYBE-NOTHING method takes
  ;; care of integrating the collected ERROR-WADs into the wad tree.
  (multiple-value-bind (object kind wad)
      (with-error-recording ()
        (eclector.reader:read-maybe-nothing client analyzer nil nil))
    (declare (ignore object))
    (case kind
      (:eof)        ; nothing to do for end of input
      (:whitespace) ; nothing to do for whitespace
      (t            ; got a top-level, absolute wads
       (if (null wad) ; or possibly nothing, but only if KIND is `:skip'
           (assert (eq kind :skip))
           (push-to-prefix (cache analyzer) wad))))
    (values kind wad)))

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
                               (read-and-cache analyzer client))
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
            do (drain-result-list analyzer cache residue pop-from-residue)
               (when (null (residue cache))
                 (drain-result-list analyzer cache suffix pop-from-suffix)))))

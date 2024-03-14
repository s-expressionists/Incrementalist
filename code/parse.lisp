(cl:in-package #:incrementalist)

(defun make-error-wad (condition start-line start-column height width)
  (let ((end-column (+ start-column width)))
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

;;; Establish a handler which collects READER-ERRORs into *ERRORS* and
;;; recovers around BODY.
(defmacro with-error-recording (() &body body)
  `(handler-bind
       ((reader-error
          (lambda (condition)
            (destructuring-bind (line . column)
                (eclector.base:stream-position condition)
              (let* ((start-column (max 0 (+ column (eclector.base:position-offset condition))))
                     (width        (eclector.base:range-length condition))
                     (error-wad    (make-error-wad
                                    condition line start-column 0 width)))
                (push error-wad *errors*)))
            (eclector.reader:recover))))
     ,@body))

(defmethod reader:read-maybe-nothing :around
    ((client client) (stream analyzer) eof-error-p eof-value)
  (let ((cached (cached-wad stream)))
    (if (or (null cached)
            ;; Can't use zero length cached wad (can happen for
            ;; error-wad) since the ADVANCE-STREAM-TO-BEYOND-WAD would
            ;; not advance in that case and the read loop would not
            ;; make any progress.
            (and (= (start-line cached) (end-line cached))
                 (= (start-column cached) (end-column cached))))
        ;; Nothing has been cached, so call
        ;; READ-MAYBE-NOTHING. Collect errors in *ERRORS* and
        ;; integrate them into the wad tree.
        (let ((*errors* '()))
          (multiple-value-bind (object kind result)
              (call-next-method)
            (when (and (not (null result)) ; RESULT can be `null' for `:skip'
                       (member kind '(:object :skip)))
              (alexandria:when-let ((reversed-errors *errors*))
                (loop for base = (start-line result) then start-line
                      for error-wad in (nreverse reversed-errors)
                      for start-line = (absolute-to-relative error-wad base)
                      collect error-wad into errors
                      finally (setf (errors result) errors))))
            (values object kind result)))
        ;; There is a cached wad for the current input position. Turn
        ;; the wad into appropriate return values, inject it into
        ;; Eclector's result stack and advance STREAM.
        (multiple-value-prog1
            (etypecase cached
              (non-cst-wad (values nil              :skip   cached t))
              (cst-wad     (values (cst:raw cached) :object cached t)))
          (push cached (first eclector.parse-result::*stack*)) ; HACK
          (advance-stream-to-beyond-wad stream cached)))))

(defun parse-and-cache (analyzer client)
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

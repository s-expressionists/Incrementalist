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

(defun make-dummy-result-for-errors (stream errors)
  ;; If errors occurred but no result was produced (an isolated . is
  ;; an example of this since Eclector's `recover' restart will skip
  ;; the invalid token without producing a result), make a dummy
  ;; result to attach the errors to.
  (let (start-line start-column end-line end-column)
    (mapc (lambda (error)
            (assert (not (relative-p error)))
            (when (or (null start-line)
                      (%position< (start-line error) (start-column error)
                                  start-line         start-column))
              (setf start-line   (start-line   error)
                    start-column (start-column error)))
            (when (or (null end-line)
                      (%position> (end-line error) (end-column error)
                                  end-line         end-column))
              (setf end-line   (end-line   error)
                    end-column (end-column error))))
          errors)
    (basic-wad* 'reader-macro-wad stream
                start-line start-column
                end-line   end-column)))

;;; The `read-maybe-nothing' method collects Eclector errors in
;;; *ERRORS* during reading.
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
    ((client client) (stream analyzer) (eof-error-p t) (eof-value t))
  (let ((cached (cached-wad stream)))
    (if (or (null cached)
            ;; Can't use zero length cached wad (can happen for
            ;; `error-wad') since the `advance-stream-to-beyond-wad'
            ;; would not advance in that case and the read loop would
            ;; not make any progress.
            (and (zerop (height cached))
                 (= (start-column cached) (end-column cached))))
        ;; Nothing has been cached, so call `read-maybe-nothing'.
        (let (object kind result
              (*errors* '()))
          ;; Collect errors into *ERRORS* as well as defined, used and
          ;; escaping cells into the respective variable. Later,
          ;; integrate the collected things into the RESULT.
          (dep:with-cell-dependencies (add-dependencies)
            (setf (values object kind result) (call-next-method))
            (when (not (null result))
              (add-dependencies result)))
          (let* ((errors *errors*)
                 (result (if (and (not (null errors)) (null result))
                             (make-dummy-result-for-errors stream errors)
                             result)))
            (when (not (null result)) ; RESULT can be `null' for KIND `:skip'
              (dbg:log :state "Got fresh wad ~A and errors ~A~%" result errors)
              (when (member kind '(:object :skip))
                (set-errors result errors))
              ;; Put defined and escaping cells into escaping cells for
              ;; surround `read' call (if any).
              (dep:register-escaping result))
            (values object kind result)))
        ;; There is a cached wad for the current input position.  Turn
        ;; the wad into appropriate return values, inject it into
        ;; Eclector's result stack and advance STREAM.
        (multiple-value-prog1
            (etypecase cached
              (read-suppress-wad (values nil              :suppress cached t))
              (non-cst-wad       (values nil              :skip     cached t))
              (cst-wad           (values (cst:raw cached) :object   cached t)))
          ;; STREAM has to be advanced before the stream line and item
          ;; number are read below.
          (advance-stream-to-beyond-wad stream cached)
          (dbg:log :state "Using restored wad ~A~%" cached)
          ;; Put defined and escaping cells into escaping cells for
          ;; parent.
          (dep:register-escaping cached)
          ;; Cells that escape from the restored wad CACHED may now
          ;; appear between some definition and a user, so invalidate
          ;; users of such definitions.
          (dep:map-external
           (lambda (cell)
             (dbg:log :invalidation "  Invalidating parent users for escaping cell ~A~%" cell)
             ;; We could process the same aspect twice but according
             ;; to an instrumented run of the test suite, that issue
             ;; is rare enough to not matter.
             (let ((aspect (dep:aspect cell)))
               (alexandria:when-let ((parent (dep:find-cell aspect)))
                 (invalidate-following-users parent))))
           cached)
          ;; Clear list of inherited cells in case CACHED used to be a
          ;; top-level wad but will now become a child of some other
          ;; wad.
          (setf (dep:inherited cached) '())
          ;; Install state from cells that are defined, inherited,
          ;; etc. by CACHED.
          (dep:install-state-from-user cached nil)
          ;;
          (push cached (first eclector.parse-result::*stack*)) ; HACK
          ))))

(defun read-and-cache-top-level-expression (analyzer client)
  ;; Use `eclector.reader:read-maybe-nothing' to read either a single
  ;; skipped input or an object. Both are pushed into the prefix of
  ;; the cache that is associated with ANALYZER.
  ;;
  ;; For any errors during parsing, `with-error-recording' creates an
  ;; `error-wad', records it in `*errors*' and then asks Eclector to
  ;; perform the appropriate recovery.  The `read-maybe-nothing'
  ;; method takes care of integrating the collected `error-wad's into
  ;; the wad tree.
  (multiple-value-bind (object kind wad)
      (with-error-recording ()
        (eclector.reader:read-maybe-nothing client analyzer nil nil))
    (declare (ignore object))
    ;; After reading a top-level expression, pop state cells that have
    ;; top-level-expression scope but not global scope.
    (dep:pop-local-cells)
    (case kind
      (:eof)          ; nothing to do for end of input
      (:whitespace)   ; nothing to do for whitespace
      (t              ; got a top-level, absolute wad
       (if (null wad) ; or possibly nothing, but only if KIND is `:skip'
           (assert (eq kind :skip))
           (let ((cache (cache analyzer)))
             ;; For the "escaping" cells of WAD, disregard those that
             ;; have top-level-expression scope since WAD represents a
             ;; toplevel expression and there is no further escaping
             ;; beyond WAD for those cells.  For the disregarded
             ;; cells, invalidate users that follow in the buffer text
             ;; since there may have been a toplevel expression which
             ;; contained WAD and also some of the those users.  In
             ;; that case, cells which previously "escaped" from WAD
             ;; and could be used in those users are now no longer
             ;; available and thus the users must be invalidated.
             (dep:update-escaping-for-toplevel-user
              wad #'invalidate-following-users)
             ;; Add the WAD to the prefix of CACHE.  `push-to-prefix'
             ;; sets the inherited cells of WAD either to the
             ;; inheritable cells of the preceding wad or the initial
             ;; reader state.  The inherited cells are used when
             ;; parsing starts after WAD and a reader state for that
             ;; location has to be computed.
             (when (null (prefix cache))
               (setf (dep:inherited wad) (initial-reader-state analyzer)))
             (push-to-prefix cache wad)))))
    (values kind wad)))

(defun read-forms (analyzer)
  (let* ((client (make-instance 'client :stream* analyzer))
         (cache  (cache analyzer))
         (prefix (prefix cache)))
    (update-lines-cache analyzer (lines analyzer))
    ;; Position ANALYZER (which is a stream) after the last prefix wad
    ;; (remember the cache prefix is stored in reverse order) if any.
    (setf (values (line-number analyzer) (item-number analyzer))
          (if (null prefix)
              (values 0 0)
              (let ((first (first prefix)))
                (values (end-line first) (end-column first)))))
    ;; Set up the reader state for the following `read' calls.  If no
    ;; wad in CACHE precedes the buffer location of ANALYZER, install
    ;; the initial reader state which is stored in ANALYZER (and was
    ;; provided by the client).
    (dep:with-cells ()
      (dep:install-state (initial-reader-state analyzer))
      (when (not (null prefix))
        (dep:install-state-from-user (first prefix) t))
      ;; Keep reading top-level expressions until the stream position
      ;; is at the start of the cache suffix (wads on the cache suffix
      ;; are unmodified and relative except the first one, so neither
      ;; re-`read'ing nor adjusting them is necessary) or the EOF is
      ;; reached.  Remove wads from the cache residue and cache suffix
      ;; as the stream positions passes them by.  Push newly created
      ;; wads to the cache prefix (this happens in
      ;; `read-and-cache-top-level-expression').
      (loop :with *cache* = cache
            :for kind = (progn
                          (dbg:log :read "at ~D:~D~%"
                                   (line-number analyzer) (item-number analyzer))
                          (multiple-value-bind (kind wad orphan-result)
                              (eclector.reader:call-as-top-level-read
                               client (lambda ()
                                        (read-and-cache-top-level-expression
                                         analyzer client))
                               analyzer t nil nil)
                            (dbg:log :read "~A~%  wad    ~A~%  orphan ~:A~%"
                                     kind wad orphan-result)
                            (when (eq kind :object)
                              (apply-semantics client wad))
                            kind))
            ;; If we reach EOF while reading whitespace, the cache
            ;; suffix must be empty, and the cache residue is either
            ;; empty or it contains wads that should be removed.  If
            ;; we do not reach EOF, then we stop only if the current
            ;; position is that of the first parse result on the cache
            ;; suffix.
            :when (or (eq kind :eof)
                      (let ((suffix (suffix cache)))
                        (and (not (null suffix))
                             (zerop (suffix-invalid-count cache))
                             (position= (first suffix) analyzer))))
              :do (alexandria:when-let ((residue (residue cache)))
                    (mapc #'detach-wad residue)
                    (setf (residue cache) '()))
                  (return)
            ;; In case we skipped some whitespace, discard any wads on
            ;; the cache residue and cache suffix that are now before
            ;; the current stream position.
            :unless (eq kind :whitespace)
              :do (drain-result-list analyzer cache residue pop-from-residue)
                  (when (null (residue cache))
                    (drain-result-list analyzer cache suffix pop-from-suffix))))
    ;; If the buffer changed such that the prefix was previously not
    ;; empty and is now empty and the suffix is non-empty, ensure that
    ;; the first element of the suffix has the initial reader state as
    ;; its set of inherited cells.
    (when (null (prefix cache))
      (alexandria:when-let ((suffix-top (first (suffix cache))))
        (when (eq (dep:inherited suffix-top) :invalid)
          (setf (dep:inherited suffix-top) (initial-reader-state analyzer)))))))

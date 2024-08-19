(cl:in-package #:incrementalist)

;;; Initial reader state

(defun make-initial-reader-state
    (&key (readtable                 eclector.reader:*readtable*)
          (package                   "COMMON-LISP-USER")
          (read-base                 10)
          (read-default-float-format 'single-float)
          (read-eval                 t)
          (features                  *features*))
  ;; Should be ordered roughly by access frequency.
  `(;; Reader state aspects that do not correspond to standard
    ;; variables.
    (eclector.reader::*consing-dot-allowed-p* . nil)
    (eclector.reader::*quasiquotation-state*  . (nil . nil))
    (eclector.reader::*quasiquotation-depth*  . 0)
    ;; Reader state aspect that correspond to standard variables.
    (*readtable*                              . ,readtable)
    (*read-suppress*                          . nil)
    (*package*                                . ,package)
    (*read-base*                              . ,read-base)
    (*read-default-float-format*              . ,read-default-float-format)
    (*read-eval*                              . ,read-eval)
    (*features*                               . ,features)
    ;; Labeled objects are not a reader state aspect from Eclector's
    ;; point of view.
    (labeled-objects                          . ())))

(defun merge-initial-reader-states (delta-state base-state)
  (if (eq delta-state base-state)
      base-state
      (loop :for (aspect . value) :in base-state
            :for new-cell = (assoc aspect delta-state)
            :collect (cons aspect (if (not (null new-cell))
                                      (cdr new-cell)
                                      value)))))

(defclass analyzer (buffer-stream)
  ((%buffer               :initarg  :buffer
                          :reader   buffer)
   (%cache                :initarg  :cache
                          :reader   cache)
   ;; Specifies the reader state that should be used for reading the
   ;; first toplevel expression of the buffer. The value is a list of
   ;; `cell's.
   (%initial-reader-state :type     #+incrementalist-debug dep:list-of-cells-for-distinct-aspects
                                    #-incrementalist-debug list
                          :reader   initial-reader-state))
  (:default-initargs
   :buffer               (alexandria:required-argument :buffer)
   :lines                nil
   :initial-reader-state (make-initial-reader-state)))

(defmethod shared-initialize :around
    ((instance   analyzer)
     (slot-names t)
     &rest args
     &key (buffer               nil buffer-supplied-p)
          (cache                nil cache-supplied-p)
          (initial-reader-state nil initial-reader-state-supplied-p))
  (prog1
      (cond ((not buffer-supplied-p)
             (call-next-method))
            (cache-supplied-p
             (let ((lines (lines cache)))
               (apply #'call-next-method instance slot-names :lines lines
                      args)))
            (t
             (let* ((cache (make-instance 'cache :buffer buffer))
                    (lines (lines cache)))
               (apply #'call-next-method instance slot-names
                      :cache cache :lines lines args))))
    ;; If supplied, use aspects from INITIAL-READER-STATE to add
    ;; aspects to or change aspects in the default initial reader
    ;; state.
    (when initial-reader-state-supplied-p
      (loop :for (aspect . value) :in (merge-initial-reader-states
                                       initial-reader-state
                                       (make-initial-reader-state))
            :collect (dep:make-cell :global aspect value) :into cells
            :finally (setf (slot-value instance '%initial-reader-state)
                           cells)))))

;;;

(defmethod position< ((left basic-wad) (right analyzer))
  (%position< (start-line left)   (start-column left)
              (line-number right) (item-number right)))

(defmethod position> ((left basic-wad) (right analyzer))
  (%position> (start-line left)   (start-column left)
              (line-number right) (item-number right)))

(defmethod position= ((left basic-wad) (right analyzer))
  (and (= (start-line left)   (line-number right))
       (= (start-column left) (item-number right))))

(defun advance-stream-to-beyond-wad (analyzer wad)
  (setf (line-number analyzer) (end-line wad)
        (item-number analyzer) (end-column wad)))

;;; Check whether there is a cached wad with a start position that
;;; corresponds to the current stream position of ANALYZER, and if so,
;;; return that wad.  If there is no such parse result, then return
;;; NIL.  If there are cached wads that entirely precede the current
;;; stream position, then remove them.
;;;
;;; If we return a wad at all, it has the following properties:
;;; 1. it is valid (`dep:validp' returns true)
;;  2. all its descendants are valid
;;; 3. it is absolute
;;; 4. all its descendants are relative
;;; 5. its absolute start line number is up-to-date
;;; 6. the absolute start line numbers are up-to-date for all its
;;;    descendants
(defun cached-wad (analyzer)
  ;; When a wad is removed here (due to being invalid or not matching
  ;; the stream position of ANALYZER), the wad and its descendants are
  ;; removed/detached from multiple data structures:
  ;; 1. The wad is popped from the residue or suffix (The descendants
  ;;    of the wad may still be on the residue or worklist)
  ;; 2. The wad and its descendants are removed from user lists of any
  ;;    cells
  ;; 3. Any cells that are defined by the wad or its descendants
  ;;    become invalid
  (let ((cache (cache analyzer)))
    (macrolet
        ((maybe-result (reader popper)
           `(progn
              (drain-result-list analyzer cache ,reader ,popper)
              (let ((rest (,reader cache))
                    wad validp)
                (cond ((null rest)
                       nil)
                      ((not (position= (setf wad (first rest)) analyzer))
                       nil)
                      ((and (not (setf validp (dep:validp wad)))
                            (not (dep:valid-for-state-p wad)))
                       (detach-wad wad)
                       (,popper cache)
                       nil)
                      (t
                       (let ((wad (,popper cache)))
                         (dbg:log :wad "Preparing wad for reuse ~A~%" wad)
                         (when (not validp) ; not valid but valid for state
                           (update-invalid-cells wad))
                         (compute-absolute-line-numbers wad))))))))
      (let ((residue-wad (maybe-result residue pop-from-residue)))
        (if (not (null residue-wad))
            residue-wad
            (maybe-result suffix pop-from-suffix))))))

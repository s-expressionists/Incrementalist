(cl:in-package #:incrementalist)

;;; We do not use the line object from Cluffer directly, because the
;;; contents of such a line may change after we have asked for it, so
;;; that we get a different contents each time we ask for it.  But we
;;; still need the line object from Cluffer, because that one is used
;;; as a comparison in the update protocol.  The solution is to have
;;; two parallel editable sequences, one containing Cluffer lines and
;;; the other containing strings.  The sequence containing strings is
;;; then used as an input to the parser.

(defclass cache ()
  (;; This slot contains the Cluffer buffer that is being analyzed by
   ;; this cache instance.
   (%buffer               :initarg  :buffer
                          :reader   buffer)
   ;; The time stamp passed to and returned by the Cluffer update
   ;; protocol.
   (%time-stamp           :accessor time-stamp
                          :type     (or null alexandria:non-negative-integer)
                          :initform nil)
   ;; This slot contains a list that parallels the prefix and it
   ;; contains the width of the prefix starting with the first element
   ;; of the prefix.
   (%lines                :reader   lines
                          :initform (make-instance 'flx:standard-flexichain))
   (%cluffer-lines        :reader   cluffer-lines
                          :initform (make-instance 'flx:standard-flexichain))
   ;; The prefix contains top-level wads in reverse order, so that the
   ;; last wad in the prefix is the first wad in the buffer.  Every
   ;; top-level wad in the prefix has an absolute line number.
   (%prefix               :accessor prefix
                          :type     list
                          :initform '())
   (%prefix-width         :accessor prefix-width
                          :initform '())
   ;; The suffix contains top-level wads in the right order.  The
   ;; first top-level wad on the suffix has an absolute line number.
   ;; All the others have relative line numbers.
   (%suffix               :accessor suffix
                          :type     list
                          :initform '())
   (%suffix-invalid-count :accessor suffix-invalid-count
                          :type     alexandria:non-negative-integer
                          :initform 0)
   ;; This slot contains a list that parallels the suffix and it
   ;; contains the width of the suffix starting with the first element
   ;; of the suffix.
   (%suffix-width         :accessor suffix-width
                          :initform '())
   ;; The residue is normally empty.  The `scavenge' phase puts orphan
   ;; wads that are still valid on the residue, and these are used by
   ;; the `read-forms' phase to avoid reading characters when the
   ;; result is known.
   (%residue              :accessor residue
                          :type     list
                          :initform '())
   (%worklist             :accessor worklist
                          :type     list
                          :initform '()))
  (:default-initargs
   :buffer (alexandria:required-argument :buffer)))

(defmethod print-object ((object cache) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~:D line~:P → prefix ~:D suffix ~:D"
            (flx:nb-elements (lines object))
            (length (prefix object))
            (length (suffix object)))))

(defmethod describe-object ((object cache) stream)
  (princ object stream)
  (let* ((lines  (lines object))
         (lines  (loop for i below (flx:nb-elements lines)
                       collect (list i (flx:element* lines i))))
         (prefix (reverse (prefix object)))
         (suffix (suffix object)))
   (format stream "~2%~
                   ~@<Lines (~:D)~@:_~
                   ~<  ~@;~:[«no wads»~;~:*~{~{~4D│~A│~}~^~@:_~}~]~:>~@:_~
                   Prefix (~:D wad~:P)~@:_~
                   ~2@T~<~:[«no wads»~;~:*~{~A~^~@:_~}~]~:>~@:_~
                   Suffix (~:D wad~:P)~@:_~
                   ~2@T~<~:[«no wads»~;~:*~{~A~^~@:_~}~]~:>~@:_~
                   ~:>"
           (length lines) (list lines)
           (length prefix) (list prefix)
           (length suffix) (list suffix))))

(defun gap-start (cache)
  (let ((prefix (prefix cache)))
    (if (null prefix)
        0
        (1+ (end-line (first prefix))))))

(defun gap-end (cache)
  (let ((suffix (suffix cache)))
    (1- (if (null suffix)
            (line-count cache)
            (start-line (first suffix))))))

(defun total-width (cache)
  (let ((prefix-width (prefix-width cache))
        (suffix-width (suffix-width cache)))
    (max (if (null prefix-width) 0 (first prefix-width))
         (max-line-length cache (gap-start cache) (gap-end cache))
         (if (null suffix-width) 0 (first suffix-width)))))

;;; Given a cache and an interval of lines, return the maxium length
;;; of any lines in the interval.
(defun max-line-length (cache first-line-number last-line-number)
  (loop for line-number from first-line-number to last-line-number
        maximize (line-length cache line-number)))

(defmethod (setf dep:invalid) :around ((new-value t) (wad wad))
  ;; If WAD is top-level (that is, it does not have a parent) and the
  ;; validity of WAD changes and WAD is on the suffix of the
  ;; associated cache, adjust the `suffix-invalid-count' of the cache.
  (if (null (parent wad))
      (let ((old-value (dep:invalid wad)))
        (flet ((maybe-adjust (amount)
                 (let ((cache (cache wad)))
                   (when (or (relative-p wad)
                             (eq wad (first (suffix cache))))
                     (incf (suffix-invalid-count cache) amount)))))
          (cond ((and (null old-value) (not (null new-value)))
                 (maybe-adjust 1))
                ((and (not (null old-value)) (null new-value))
                 (maybe-adjust -1))))
        (call-next-method))
      (call-next-method)))

(defgeneric pop-from-suffix (cache)
  (:method ((cache cache))
    (let* ((old-suffix     (suffix cache))
           (old-suffix-top (first old-suffix))
           (new-suffix     (rest old-suffix))
           (new-suffix-top (first new-suffix)))
      (assert (not (null old-suffix)))
      (pop (suffix-width cache))
      (unless (null (dep:invalid old-suffix-top))
        (decf (suffix-invalid-count cache)))
      (setf (suffix cache) new-suffix)
      (unless (null new-suffix-top)
        (relative-to-absolute new-suffix-top (start-line old-suffix-top)))
      (link-siblings (first (prefix cache)) new-suffix-top)
      old-suffix-top)))

(defgeneric push-to-suffix (cache wad)
  (:method ((cache cache) (wad wad))
    (assert (not (relative-p wad)))
    (let* ((old-suffix       (suffix cache))
           (old-suffix-top   (first old-suffix))
           (new-suffix       (list* wad old-suffix))
           (old-suffix-width (suffix-width cache))
           (new-width        (max-line-width wad)))
      (if (null old-suffix-top)
          (alexandria:maxf
           new-width
           (max-line-length cache (1+ (end-line wad)) (1- (line-count cache))))
          (progn
            (absolute-to-relative old-suffix-top (start-line wad))
            (alexandria:maxf
             new-width
             (first old-suffix-width)
             (max-line-length
              cache (1+ (end-line wad)) (1- (start-line old-suffix-top))))))
      (unless (null (dep:invalid wad))
        (incf (suffix-invalid-count cache)))
      (setf (suffix-width cache) (list* new-width old-suffix-width)
            (suffix cache)       new-suffix)
      (link-siblings (first (prefix cache)) wad)
      (link-siblings wad                    old-suffix-top))))

(defgeneric pop-from-prefix (cache)
  (:method ((cache cache))
    (let* ((old-prefix     (prefix cache))
           (old-prefix-top (first old-prefix))
           (new-prefix     (rest old-prefix))
           (new-prefix-top (first new-prefix)))
      (assert (not (null old-prefix)))
      (pop (prefix-width cache))
      (setf (prefix cache) new-prefix)
      (link-siblings new-prefix-top (first (suffix cache)))
      old-prefix-top)))

(defgeneric push-to-prefix (cache wad)
  (:method ((cache cache) (wad wad))
    (let* ((old-prefix       (prefix cache))
           (old-prefix-top   (first old-prefix))
           (new-prefix       (list* wad old-prefix))
           (old-prefix-width (prefix-width cache))
           (old-width        (first old-prefix-width))
           (new-width        (max-line-width wad)))
      (if (not (null old-prefix-top))
          (setf (dep:inherited wad) (dep:inheritable old-prefix-top))
          (assert (not (eq :invalid (dep:inherited wad)))))
      (if (not (null old-prefix-top))
          (alexandria:maxf
           new-width
           old-width
           (max-line-length
            cache (1+ (end-line old-prefix-top)) (1- (start-line wad))))
          (alexandria:maxf
           new-width (max-line-length cache 0 (1- (start-line wad)))))
      (setf (prefix-width cache) (list* new-width old-prefix-width)
            (prefix       cache) new-prefix)
      (link-siblings old-prefix-top wad)
      (link-siblings wad            (first (suffix cache))))))

(defgeneric suffix-to-prefix (cache)
  (:method ((cache cache))
    (let ((wad (pop-from-suffix cache)))
      ;; WAD has been taken from the suffix so
      ;; 1) the absolute line numbers in WAD itself and its
      ;;    descendants are potentially invalid.
      ;; 2) the list of inherited cells in WAD is not populated
      ;; We handle 1) here while 2) is handled by `push-to-prefix'.
      (compute-absolute-line-numbers wad)
      (push-to-prefix cache wad))))

(defgeneric prefix-to-suffix (cache)
  (:method ((cache cache))
    (let ((wad (pop-from-prefix cache)))
      ;; During the time WAD will spend on the suffix, unless the
      ;; prefix is empty, its list of inherited cells would not be
      ;; kept up-to-date.  So we clear the slot here and recompute a
      ;; new value when WAD is moved to the prefix.
      (unless (null (prefix cache))
        (setf (dep:inherited wad) :invalid))
      (push-to-suffix cache wad))))

(defun pop-from-worklist (cache)
  (pop (worklist cache)))

(defun push-to-worklist (cache wad)
  (push wad (worklist cache)))

(defun pop-from-residue (cache)
  (pop (residue cache)))

(defun push-to-residue (cache wad)
  (push wad (residue cache)))

(defmacro drain-result-list (analyzer cache reader popper)
  `(loop :for remaining = (,reader ,cache)
         :while (and (not (null remaining))
                     (position< (first remaining) ,analyzer))
         ;; Detach before popping because detaching may traverse the
         ;; ancestors of the wad and popping may sever the parent
         ;; link.
         :do (detach-wad (first remaining))
             (,popper ,cache)))

(defun finish-scavenge (cache)
  ;; Move entire worklist to residue
  (loop until (null (worklist cache))
        do (push-to-residue cache (pop-from-worklist cache)))
  (setf (residue cache) (nreverse (residue cache))))

;;; This function is called by the three operations that handle
;;; modifications.  The first time this function is called, we must
;;; position the prefix and the suffix according to the number of
;;; lines initially skipped.
(defun ensure-update-initialized (cache line-counter)
  ;; As long as there are wads on the prefix that do not completely
  ;; precede the number of skipped lines, move them to the suffix.
  (loop for prefix = (prefix cache)
        while (and (not (null prefix))
                   (>= (end-line (first prefix)) line-counter))
        do (prefix-to-suffix cache))
  ;; As long as there are wads on the suffix that completely precede
  ;; the number of skipped lines, move them to the prefix.
  (loop for suffix = (suffix cache)
        while (and (not (null suffix))
                   (< (end-line (first suffix)) line-counter))
        do (suffix-to-prefix cache)))

;;; Return true if and only if either there are no more wads, or the
;;; first wad starts at a line that is strictly greater than
;;; LINE-NUMBER.
(defun next-wad-is-beyond-line-p (cache line-number)
  (let ((worklist (worklist cache)))
    (if (not (null worklist))
        (> (start-line (first worklist)) line-number)
        (let ((suffix (suffix cache)))
          (or (null suffix)
              (> (start-line (first suffix)) line-number))))))

;;; Return true if and only if LINE-NUMBER is one of the lines of WAD.
;;; The START-LINE of WAD is an absolute line number.
(defun line-is-inside-wad-p (wad line-number)
  (let ((start-line (start-line wad)))
    (and (<= start-line line-number)
         (<= line-number (+ start-line (height wad))))))

;;; Add INCREMENT to the absolute line number of every wad on the
;;; worklist, and of the first wad of the suffix, if any.
(defun adjust-worklist-and-suffix (cache increment)
  (flet ((adjust-wad (wad amount)
           (let ((new-start-line (+ (start-line wad) amount)))
             (setf (start-line wad)          new-start-line
                   (absolute-start-line wad) new-start-line))
           wad))
    (loop for wad in (worklist cache)
          do (adjust-wad wad increment))
    (alexandria:when-let ((suffix (suffix cache)))
      (adjust-wad (first suffix) increment))))

;;; If the worklist is empty then move a wad from the suffix to the
;;; worklist (in that case, it is known that the suffix is not empty).
(defun ensure-worklist-not-empty (cache)
  (when (null (worklist cache))
    (push-to-worklist cache (pop-from-suffix cache))))

;;; When this function is called, there is at least one wad, either on
;;; the worklist or on the suffix that must be processed, i.e., that
;;; wad either entirely precedes LINE-NUMBER (so that it should be
;;; moved to the residue), or it straddles the line with that line
;;; number, so that it must be taken apart.
(defun process-next-wad (cache line-number)
  (ensure-worklist-not-empty cache)
  (let ((wad (pop-from-worklist cache)))
    (if (line-is-inside-wad-p wad line-number)
        (let ((children (children wad)))
          (detach-wad wad :recursive nil)
          (make-absolute children (start-line wad))
          (setf (worklist cache) (append children (worklist cache))))
        (push-to-residue cache wad))))

(defun handle-modified-line (cache line-number)
  (let* ((cluffer-line (flx:element* (cluffer-lines cache) line-number))
         (string       (coerce (cluffer:items cluffer-line) 'string)))
    (setf (flx:element* (lines cache) line-number) string))
  (loop until (next-wad-is-beyond-line-p cache line-number)
        do (process-next-wad cache line-number)))

(defun handle-inserted-line (cache line-number)
  (loop until (next-wad-is-beyond-line-p cache (1- line-number))
        do (process-next-wad cache line-number))
  (adjust-worklist-and-suffix cache 1))

(defun handle-deleted-lines (cache line-number line-count)
  (loop :for line :from (- line-number line-count 1) :to line-number
        :do (loop :until (next-wad-is-beyond-line-p cache line)
                  :do (process-next-wad cache line)))
  (adjust-worklist-and-suffix cache (- line-count)))

;;; Take into account modifications to the buffer by destroying the
;;; parts of the cache that are no longer valid, while keeping parse
;;; results that are not affected by such modifications.
(defun scavenge (cache)
  (let ((buffer              (buffer cache))
        (lines               (lines cache))
        (cluffer-lines       (cluffer-lines cache))
        (cache-initialized-p nil)
        (line-counter        0))
    (labels ((ensure-cache-initialized ()
               (unless cache-initialized-p
                 (setf cache-initialized-p t)
                 (ensure-update-initialized cache line-counter)))
             ;; Line deletion
             (delete-cache-lines (start-line-number end-line-number)
               (let ((count (- end-line-number start-line-number)))
                 (loop :repeat count
                       :do (flx:delete* lines start-line-number)
                           (flx:delete* cluffer-lines start-line-number))
                 (handle-deleted-lines cache (1- end-line-number) count)))
             (remove-deleted-lines (line)
               ;; Look at cache lines starting at LINE-COUNTER. Delete
               ;; all cache lines that do not have LINE as their
               ;; associated cluffer line. Those lines correspond to
               ;; deleted lines between the previously processed line
               ;; and LINE.
               (loop :for end-line-number :from line-counter
                     :for cluffer-line
                        = (flx:element* cluffer-lines end-line-number)
                     :until (eq line cluffer-line)
                     :finally (when (< line-counter end-line-number)
                                (delete-cache-lines
                                 line-counter end-line-number))))
             ;; Handlers for Cluffer's update protocol events.
             (skip (count)
               (incf line-counter count))
             (modify (line)
               (ensure-cache-initialized)
               (remove-deleted-lines line)
               (handle-modified-line cache line-counter)
               (incf line-counter))
             (create (line)
               (ensure-cache-initialized)
               (let ((string (coerce (cluffer:items line) 'string)))
                 (flx:insert* lines line-counter string)
                 (flx:insert* cluffer-lines line-counter line))
               (handle-inserted-line cache line-counter)
               (incf line-counter))
             (sync (line)
               (remove-deleted-lines line)
               (incf line-counter)))
      ;; Run update protocol. The handler functions defined above
      ;; change the cache lines and the worklist so that they
      ;; correspond to the new buffer state.
      (setf (time-stamp cache)
            (cluffer:update buffer
                            (time-stamp cache)
                            #'sync #'skip #'modify #'create))
      ;; Remove trailing cache lines after the last
      ;; skipped/modified/... cache line, that no longer correspond to
      ;; existing lines in the cluffer buffer.
      (let ((cache-line-count (flx:nb-elements lines)))
        (when (< line-counter cache-line-count)
          (delete-cache-lines line-counter cache-line-count)))))
  (finish-scavenge cache))

(defmethod line-count ((cache cache))
  (flx:nb-elements (lines cache)))

(defmethod line-length ((cache cache) line-number)
  (length (flx:element* (lines cache) line-number)))

(defmethod line-contents ((cache cache) line-number)
  (flx:element* (lines cache) line-number))

(defun map-empty-area
    (cache first-line start-column last-line end-column space-function)
  (when (%position> last-line end-column first-line start-column)
    (if (= first-line last-line)
        (funcall space-function first-line start-column end-column)
        (progn
          ;; Handle the first line.
          (let ((line-length (line-length cache first-line)))
            (when (> line-length start-column)
              (funcall space-function first-line start-column line-length)))
          ;; Handle all remaining lines except the last one.
          (loop for line-number from (1+ first-line) below last-line
                for line-length = (line-length cache line-number)
                when (> line-length 0)
                  do (funcall space-function line-number 0 line-length))
          ;; Handle the last line.
          (when (> end-column 0)
            (funcall space-function last-line 0 end-column))))))

(defun map-wads-and-spaces
    (cache first-line last-line wad-function space-function)
  ;; Make sure no wad on the suffix starts at or before LAST-LINE.
  (loop until (null (suffix cache))
        while (<= (start-line (first (suffix cache))) last-line)
        do (suffix-to-prefix cache))
  ;; Find a suffix of the prefix (i.e., a prefix of wads in the
  ;; buffer) such that it is not the case that the first wad of the
  ;; prefix (i.e., the last wad of the buffer prefix) starts entirely
  ;; after LAST-LINE.
  (let ((remaining
          (loop for remaining on (prefix cache)
                when (<= (start-line (first remaining)) last-line)
                  return remaining)))
    (if (null remaining)
        (let ((line-length (line-length cache last-line)))
          (map-empty-area
           cache first-line 0 last-line line-length space-function))
        (progn (when (<= (end-line (first remaining)) last-line)
                 (map-empty-area
                  cache
                  (end-line (first remaining))
                  (end-column (first remaining))
                  last-line
                  (line-length cache last-line)
                  space-function))
               (loop for (wad2 wad1) on remaining
                     do (funcall wad-function wad2)
                     until (or (null wad1)
                               (< (end-line wad1) first-line))
                     do (map-empty-area
                         cache
                         (end-line wad1)
                         (end-column wad1)
                         (start-line wad2)
                         (start-column wad2)
                         space-function)
                     finally
                        (if (null wad1)
                            (map-empty-area
                             cache
                             0 0
                             (start-line wad2)
                             (start-column wad2)
                             space-function)
                            (map-empty-area
                             cache
                             (end-line wad1)
                             (end-column wad1)
                             (start-line wad2)
                             (start-column wad2)
                             space-function)))))))

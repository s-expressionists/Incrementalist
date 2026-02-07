(cl:in-package #:incrementalist.test)

;;; Insert then delete, updating and checking the cache after each
;;; change

(defun check-parse-result (parse-result)
  (labels ((check-wad (wad)
             (let ((items (inc:items wad)))
               (unless (stringp items)
                 (5am:fail "Failed to get items for wad ~A" wad)))
             (inc:map-children #'check-wad wad))
           (maybe-check-wad (wad)
             (when (not (or (inc::relative-p wad)
                            (eq wad (first (inc::suffix (inc:cache wad))))))
               (check-wad wad))))
    (mapc #'maybe-check-wad parse-result)))

(defun progress-reporter (&key (stream *trace-output*) (indicator #\.))
  (if (null stream)
      (lambda (&optional finalp)
        (declare (ignore finalp)))
      (let ((i 0)
            start-time)
        (lambda (&optional finalp)
          (when (zerop i)
            (setf start-time (get-internal-real-time))
            (format stream "~&;   "))
          (when (zerop (mod i 1000))
            (when (and (plusp i)
                       *print-right-margin*
                       (zerop (mod (/ i 1000) *print-right-margin*)))
              (format stream "~&;   "))
            (write-char indicator stream)
            (force-output stream))
          (incf i)
          (when finalp
            (let ((end-time (get-internal-real-time)))
              (values (/ (- end-time start-time)
                         internal-time-units-per-second)
                      i)))))))

(defun insert-then-delete (content &key (stream *trace-output*))
  (let (count insert-time delete-time)
    (multiple-value-bind (analyzer cache buffer cursor) (prepared-analyzer "")
      ;; Insert
      (loop :with reporter = (progress-reporter :indicator #\+ :stream stream)
            :for character across content
            :do (funcall reporter)
            :do (case character
                  (#\Newline (b:split-line cursor))
                  (t         (b:insert-item-at cursor character)))
                (let ((result (update-cache analyzer cache)))
                  (check-parse-result result))
            :finally (setf (values insert-time count) (funcall reporter t)))
      ;; Delete
      (b:detach cursor)
      (b:attach cursor (b:find-line buffer 0))
      (loop :with reporter = (progress-reporter :indicator #\- :stream stream)
            :while (or (> (b:line-count buffer) 1) ; fast check
                       (plusp (b:item-count buffer)))
            :for i :from 0
            :do (funcall reporter)
            :do (if (b:end-of-line-p cursor)
                    (b:join-line cursor)
                    (b:delete-item-after cursor))
                (let ((result (update-cache analyzer cache)))
                  (check-parse-result result))
            :finally (setf delete-time (funcall reporter t)))
      ;; Buffer is empty.
      (is-true (a:emptyp (buffer-string buffer)))
      (let ((result (update-cache analyzer cache)))
        ;; Parse result list is empty.
        (is (null result))
        ;; Check initial reader state for dangling user references.
        (no-dangling-nodes '() analyzer)))
    (values count insert-time delete-time)))

;;; Reading code from files

(defun insert-then-delete-file (filename &key (stream *trace-output*))
  (format stream "~&; ~A~%" (file-namestring filename))
  (let ((content (a:read-file-into-string filename)))
    (multiple-value-bind (count insert-time delete-time)
        (insert-then-delete content :stream stream)
      (format stream "~&;   insert: ~,2F s~@[ (~:D/s)~] ~
                            delete: ~,2F s~@[ (~:D/s)~]~%"
              insert-time (when (plusp insert-time) (floor count insert-time))
              delete-time (when (plusp delete-time) (floor count delete-time))))))

(defun map-all-system-files (function
                             &key (systems '("incrementalist"
                                             "incrementalist/test")))
  (labels ((map-component-files (component)
             (typecase component
               (asdf:source-file
                (when (equal (asdf:file-type component) "lisp")
                  (let ((filename (asdf:component-pathname component)))
                    (funcall function filename))))
               (asdf:module
                (let ((children (asdf:component-children component)))
                  (mapc #'map-component-files children))))))
    (mapc (lambda (system-name)
            (let ((system (asdf:find-system system-name)))
              (map-component-files system)))
          systems)))

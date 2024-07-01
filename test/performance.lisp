(cl:in-package #:incrementalist.test)

(def-suite* :incrementalist.performance
  :in :incrementalist)

(defun many-lines-of-nested-lists (stream count)
  (loop :for i :below count
        :when (zerop (mod (+ i 600) 600))
        :do (format stream "(")
        :do (format stream "foo (bar (baz fez) 1234)~%")
        :when (zerop (mod (+ i 300) 600))
        :do (format stream ")~%")))

(defun many-lines-of-block-comment (stream count)
  (write-string "#|" stream)
  (loop :repeat count
        :do (format stream "foo bar baz fez whoop di doo~%"))
  (write-string "|#" stream))

(defun call-with-note-time (thunk format-control &rest format-arguments)
  (let ((stream     *trace-output*)
        (start-time (get-internal-real-time)))
    (format stream "~&; ~48@<~?:~> " format-control format-arguments)
    (force-output stream)
    (multiple-value-prog1
        (funcall thunk)
      (format stream "~,2F s ~%"
              (/ (- (get-internal-real-time) start-time)
                 internal-time-units-per-second)))))

(defmacro note-time ((format-control &rest format-arguments) &body body)
  `(call-with-note-time (lambda () ,@body) ,format-control ,@format-arguments))

(defun test-delete-many-lines (content-maker line-count)
  (multiple-value-bind (analyzer cache buffer cursor)
      (note-time ("Insert ~:D lines" line-count)
        (prepared-analyzer (with-output-to-string (stream)
                             (funcall content-maker stream line-count))))
    ;; Update cache with large buffer content.
    (note-time ("Initial update after adding ~:D lines" line-count)
      (update-cache analyzer cache))
    ;; Delete buffer content.
    (note-time ("Delete ~:D lines" line-count)
      (cluffer:detach-cursor cursor)
      (cluffer:attach-cursor cursor (cluffer:find-line buffer 0))
      (loop :while (or (> (cluffer:line-count buffer) 1) ; fast check
                       (plusp (cluffer:item-count buffer)))
            :do (if (cluffer:end-of-line-p cursor)
                    (cluffer:join-line cursor)
                    (cluffer:delete-item cursor))))
    ;; Update with empty buffer.
    (note-time ("Update time after deleting ~:D lines" line-count)
      (update-cache analyzer cache))
    ;; Not important.
    (finishes)))

(test delete-many-lines.nested-lists
  "Ensure that many lines filled with nested lists can be deleted
quickly."
  (test-delete-many-lines 'many-lines-of-nested-lists 10000))

(test delete-many-lines.block-comment
  "Ensure that many lines filled with a single block comment can be
deleted quickly."
  (test-delete-many-lines 'many-lines-of-block-comment 10000))

(cl:in-package #:incrementalist.test)

(defun prepared-buffer (content)
  (let* ((line   (make-instance 'cluffer-standard-line:open-line))
         (buffer (make-instance 'cluffer-standard-buffer:buffer :initial-line line))
         (cursor (make-instance 'cluffer-standard-line:right-sticky-cursor)))
    (cluffer:attach-cursor cursor line 0)
    (loop :for c :across content
          :do (case c
                (#\Newline (cluffer:split-line cursor))
                (t         (cluffer:insert-item cursor c))))
    (values buffer cursor)))

(defun prepared-analyzer (buffer-content)
  (multiple-value-bind (buffer cursor) (prepared-buffer buffer-content)
    (let* ((cache    (make-instance 'inc::cache :cluffer-buffer buffer))
           (analyzer (make-instance 'inc::analyzer :buffer buffer
                                                   :lines  (inc:lines cache)
                                                   :cache  cache)))
      (values analyzer cache buffer cursor))))

(defun parse-result (buffer-content)
  (multiple-value-bind (analyzer cache) (prepared-analyzer buffer-content)
    (inc::update-cache analyzer)
    (append (reverse (inc::prefix cache)) (inc::suffix cache))))

(cl:in-package #:incrementalist.test)

(in-suite :incrementalist)

(test read-code
  "Incrementally insert and delete contents of all source files, parsing
after each change."
  (let ((fiveam:*test-dribble* nil)
        (*print-right-margin*  80))
    (map-all-system-files #'insert-then-delete-file)))

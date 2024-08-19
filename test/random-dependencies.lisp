(cl:in-package #:incrementalist.test)

(in-suite :incrementalist.random)

;;; This string is intended to be put into a buffer and then edited by
;;; randomly replacing the first three characters of certain lines
;;; with either ";; " or "   " in order to the test the invalidation
;;; of expressions that follow the changed location in the buffer.
(defvar *random-dependencies-input*
  "   (cl:in-package #:cl-user)
   #.(setf *read-base* 17)
;;; This is a distraction
   \"
   some words
   \"
    #+(and)
(
   #.(setf *read-base* 10)
   10
   #01=foo
   bar
   #01#
)
   `
   `
   (
    11
    foo
    ,(bar 12)
    ,,(baz 13)
   )
   #+
   foo
   bar
   #-
   baz
   fez
")

(defvar *random-dependencies-edits*
  (flet ((comment-or-uncomment (line column)
           (list `(:replace (,line ,column) ";; ")
                 `(:replace (,line ,column) "   ")))
         (change-label (line column)
           (list `(:replace (,line ,column) "01")
                 `(:replace (,line ,column) "02"))))
    (append ;; Package
            (comment-or-uncomment 0 0)
            ;; Read base
            (comment-or-uncomment 1 0)
            '((:replace (1 23) "17")
              (:replace (1 23) "10"))
            ;; String
            (comment-or-uncomment 3 0)
            (comment-or-uncomment 5 0)
            ;; Reader conditional
            (comment-or-uncomment 6 0)
            ;; Labeled objects
            (comment-or-uncomment 10 0)
            (change-label 10 4)
            (comment-or-uncomment 12 0)
            (change-label 12 4)
            ;; Backquote
            (comment-or-uncomment 14 0)
            (comment-or-uncomment 15 0)
            ;; Reader conditionals
            (loop :for line :from 22 :to 27
                  :append (comment-or-uncomment line 0)))))

(defun random-edit ()
  (a:random-elt *random-dependencies-edits*))

(defun apply-random-edit (cursor)
  (let ((edit (random-edit)))
    (apply-edit cursor edit)
    edit))

(test dependencies.random
  "Ensure that incremental and from-scratch parsing after random edits
yields identical results."
  (let ((fiveam:*test-dribble* nil))
    (loop :with (analyzer cache buffer cursor)
             = (multiple-value-list
                (prepared-analyzer *random-dependencies-input*))
          :repeat 200
          :for ()                 = (apply-random-edit cursor)
          :for content            = (buffer-string buffer)
          :for results-with-cache = (update-cache analyzer cache)
          :for results-no-cache   = (parse-result content)
          :do (no-dangling-nodes results-with-cache analyzer)
              (are-results= results-no-cache results-with-cache
                            :input content))))

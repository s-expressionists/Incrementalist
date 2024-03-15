(cl:in-package #:incrementalist.test)

(in-suite :incrementalist)

(test regressions
  "Ensure that inputs which previously caused errors are processed
correctly."
  (let ((fiveam:*test-dribble* nil))
    (flet ((test-case (input)
             (insert-then-delete input :stream nil)))
      (test-case
       "; ,
 (defun position-is-before-wad-p)
")

      (test-case "foo'")

      (test-case "(cl:in-package #:incrementalist)

              (defun whitespacep (character)
                (member character '(#\\Space #\\Newline)))

              (defun punctuationp (character)
                (member character '(#\\. #\\? #\\! #\\: #\\, #\\;
                                    #\\( #\\) #\\< #\\> #\\[ #\\] #\\{ #\\}
                                    #\\\" #\\' #\\` #\\/ #\\_ #\\- #\\+ #\\* #\\% #\\= #\\#)))

;;; Return the line number and the column number of CURSOR as two
;;; values.
              (defun cursor-positions (cursor)
                (values (cluffer:line-number cursor)
                        (cluffer:cursor-position cursor)))

;;; Set the line number and the column number of CURSOR.
              (defun set-cursor-positions (cursor line-number column-number)
                (let ((buffer (cluffer:buffer cursor)))
                  (when (cluffer:cursor-attached-p cursor)
                    (cluffer:detach-cursor cursor))
                  (cluffer:attach-cursor
                   cursor
                   (cluffer:find-line buffer line-number)
                   column-number)))
")
      (test-case
       "(
(a .
`b))")
      (test-case ";,
(a () ;
#-b c)")
      (test-case "; |#
; #|
()")

      ;; Problems around extra children in proper and dotted lists.
      (test-case "(
; a
; b
(1 . 2)
; c
; d
3")
      (test-case "((a . b) . (c .")
      (test-case "(
; a
; b
(1 . 2)
; c
.
; d
3")
      (test-case "(
; a
(1 . 2)
#||#
.
#||#
3")

      ;; Problems around invalid context for unquote.
      (test-case ",`bar")
      (test-case "`foo, `bar")

      ;; The problem was that inputs like (0 . 0) lead to a structure
      ;; in which the atom CST that represents the first 0 was used as
      ;; both the CST first and CST rest, thus appearing twice in the
      ;; wad children.
      ;;
      ;; A new heuristic in Eclector now prevents this. However, the
      ;; MAP-CHILDREN method for CONS-CST is also fixed to report only
      ;; one of the FIRST and REST if they refer to the same object.
      (test-case "(; (0 . 0) . ())")
      (test-case "(0 . 0) . ())"))))

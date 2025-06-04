(cl:in-package #:incrementalist.test)

(def-suite* :incrementalist.regressions
  :in :incrementalist)

(test regressions.various
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
"))))

(test regressions.lists
  "Ensure that lists work with various bits of valid and invalid syntax."
  (flet ((test-case (input)
           (insert-then-delete input :stream nil)))
    (test-case "(
(a .
`b))")
    (test-case ";,
(a () ;
#-b c)")
    (test-case "; |#
; #|
()")))

(test regressions.non-cst-children-in-lists
  "Ensure that extra children in proper and dotted lists work."
  (flet ((test-case (input)
           (insert-then-delete input :stream nil)))
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
3")))

(test regressions.quasiquotation
  "Ensure that invalid contexts for unquote work."
  (flet ((test-case (input)
           (insert-then-delete input :stream nil)))
    (test-case ",`bar")
    (test-case "`foo, `bar")))

(test regressions.duplicate-cst-atoms
  "Ensure that certain atoms as CST children are not repeated as WAD children."
  ;; The problem was that inputs like (0 . 0) lead to a structure in
  ;; which the atom CST that represents the first 0 was used as both
  ;; the CST first and CST rest, thus appearing twice in the wad
  ;; children.
  ;;
  ;; A new heuristic in Eclector now prevents this. However, the
  ;; `map-children' method for `cons-cst' is also fixed to report only
  ;; one of the first and rest if they refer to the same object.
  (flet ((test-case (input)
           (insert-then-delete input :stream nil)))
    (test-case "(; (0 . 0) . ())")
    (test-case "(0 . 0) . ())")))

(test regressions.nested-labeled-objects
  "Ensure that nested labeled objects work."
  ;; Nested labeled object definitions lead to a situation in which an
  ;; escaping cell and a defined cell exist for the same aspect. The
  ;; escaping cell must be used when computing the "external" state
  ;; for the WAD.
  (insert-then-delete "(
 #1=(#2=foo #3=bar))" :stream nil))

(test regressions.kind-of-restored-read-suppressed-wad
  "Ensure that `read-maybe-nothing' returns the correct kind for a
`read-suppress-wad' that has been restored from the cache."
  ;; Make sure the keywords used in the test are interned.
  :a :foo :bar
  (edits-cases ()
    (;; A `read-suppress-wad' for `b' is added to the cache.
     "(
  #+a
  b
  c
)"
     `((inc:cons-wad ((0 0) (4 1)) ()
        (inc:skipped-positive-conditional-wad ((1 2) (2 3))
           (:feature-expression (inc:existing-symbol-token
                                 :symbol ("KEYWORD" "A")))
         ,(expected-symbol-wad '((1 4) (1 5)) "A"
                               :token-class  'inc:existing-symbol-token
                               :package-name "KEYWORD")
         (inc:read-suppress-wad ((2 2) (2 3))))
        ,(expected-symbol-wad '((3 2) (3 3)) "C")))
     ;; Remove the wad for the whole reader conditional from the
     ;; cache. The `read-suppress-wad' can be restored from the
     ;; cache. The following wad is processed correctly only if the
     ;; `read-maybe-nothing' call for the `read-suppress-wad' returns
     ;; the correct kind.
     '(:insert (1 0) " ")
     `((inc:cons-wad ((0 0) (4 1)) ()
        (inc:skipped-positive-conditional-wad ((1 3) (2 3))
           (:feature-expression (inc:existing-symbol-token
                                 :symbol ("KEYWORD" "A")))
         ,(expected-symbol-wad '((1 5) (1 6)) "A"
                               :token-class  'inc:existing-symbol-token
                               :package-name "KEYWORD")
         (inc:read-suppress-wad ((2 2) (2 3))))
        ,(expected-symbol-wad '((3 2) (3 3)) "C"))))
    (;; A `read-suppress-wad' for the negative conditional is added to
     ;; the cache.
     "#+foo
#-bar defun
111"
     #1=`((inc:skipped-positive-conditional-wad ((0 0) (1 11)) ()
           ,(expected-symbol-wad '((0 2) (0 5)) "FOO"
                                 :token-class 'inc:existing-symbol-token
                                 :package-name "KEYWORD")
           (inc:read-suppress-wad ((1 0) (1 11)) ()
            ,(expected-symbol-wad '((1 2) (1 5)) "BAR"
                                  :token-class  'inc:existing-symbol-token
                                  :package-name "KEYWORD")
            (inc:read-suppress-wad ((1 6) (1 11)) ())))
          (inc:atom-wad ((2 0) (2 3)) (:raw 111)))
     ;; The positive conditional is re-read. If the
     ;; `read-suppress-wad' for the negative conditional is restored
     ;; incorrectly, the final atom wad can (incorrectly, of course)
     ;; become a child of the positive conditional.
     '(:poke (0 2))
     #1#)))

(test regressions.reparent-incomplete-reader-conditional-wad
  "Ensure that an incomplete reader conditional wad can become a child of
another wad."
  (edits-cases ()
    (;; An incomplete reader conditional with two errors.
     "
#+"
     '(#1=(inc:skipped-positive-conditional-wad ((1 0) (1 2))
             (:errors ((((1 2) (1 2)) eclector.reader:end-of-input-after-sharpsign-plus-minus)
                       (((1 2) (1 2)) eclector.reader:end-of-input-after-feature-expression)))))
     ;; The reader conditional becomes a child of the wad which
     ;; represents the unterminated list. the reader conditional wad
     ;; and its error children should remain unchanged.
     '(:insert (0 0) "(")
     '((inc:atom-wad ((0 0) (1 2))
          (:errors ((((1 2) (1 2)) eclector.reader:unterminated-list)))
        #1#)))))

(test regressions.delete-first-line
  "Deleting the contents of the first buffer line could lead to invalid
inherited cells in the first element of the cache suffix."
  (edits-cases ()
    (;; A "second" line.
     "2
"
     :ignore
     ;; Add a first line before the second line.
     '(:insert (0 0) "1
")
     :ignore
     ;; Delete the contents of the first line. This removes the single
     ;; element of the prefix.
     '(:erase (0 0) 1)
     :ignore)))

(test regressions.incomplete-labeled-object-followed-by-two-comments
  "For an incomplete labeled object with only skipped input expressions
as its children, Eclector used to produce incorrect parse results."
  (edits-cases ()
    (;; Two line comments.  Completely harmless so far.
     "
;;1
;;2"
     :ignore
     ;; Add an incomplete labeled object which makes the two line
     ;; comments its children.  The problem used to be that Eclector
     ;; picked one of the comment wads as the parse result that
     ;; represents the final value of the labeled object.  The correct
     ;; result is shown below: due to error recovery in Eclector,
     ;; there is no labeled object definition at all; just the
     ;; replacement value with the comment wads as children.
     '(:insert (0 0) "#1=")
     '((inc:atom-wad ((0 0) (2 3))
          (:raw    (inc:existing-symbol-token :symbol ("COMMON-LISP" "NIL"))
           :errors ((((2 3) (2 3)) eclector.reader:end-of-input-after-sharpsign-equals)))
        (inc:semicolon-comment-wad ((1 0) (2 0)) ()
         (inc:text-wad ((1 2) (1 3))))
        (inc:semicolon-comment-wad ((2 0) (2 3)) ()
         (inc:text-wad ((2 2) (2 3))))))
     ;; Final state is the complete labeled object. This is harmless
     ;; again (except for potential problems from updating the
     ;; previous state).
     `(:insert (2 3) ,(format nil "~%(1 #1#)"))
     '((inc:labeled-object-definition-wad ((0 0) (3 7)) ()
        (inc:semicolon-comment-wad ((1 0) (2 0)) ()
         (inc:text-wad ((1 2) (1 3))))
        (inc:semicolon-comment-wad ((2 0) (3 0)) ()
         (inc:text-wad ((2 2) (2 3))))
        (inc:cons-wad ((3 0) (3 7)) ()
         (inc:atom-wad ((3 1) (3 2)) (:raw 1))
         (inc:labeled-object-reference-wad ((3 3) (3 6))))))
     ;; Not related to the original regression: Replace the labeled
     ;; object definition with a non-circular, in fact unused, one.
     '(progn (:erase (3 0) 7) (:insert (3 0) "1" ))
     '((inc:labeled-object-definition-wad ((0 0) (3 1)) ()
        (inc:semicolon-comment-wad ((1 0) (2 0)) ()
         (inc:text-wad ((1 2) (1 3))))
        (inc:semicolon-comment-wad ((2 0) (3 0)) ()
         (inc:text-wad ((2 2) (2 3))))
        (inc:atom-wad ((3 0) (3 1)) (:raw 1))))
     ;; Not related to the original regression: change to sharing
     ;; without circularity.
     '(progn (:insert (0 0) "(") (:insert (3 1) " #1#)"))
     '((inc:cons-wad ((0 0) (3 6)) ()
        (inc:labeled-object-definition-wad ((0 1) (3 1)) ()
         (inc:semicolon-comment-wad ((1 0) (2 0)) ()
          (inc:text-wad ((1 2) (1 3))))
         (inc:semicolon-comment-wad ((2 0) (3 0)) ()
          (inc:text-wad ((2 2) (2 3))))
         (inc:atom-wad ((3 0) (3 1)) (:raw 1)))
        (inc:labeled-object-reference-wad ((3 2) (3 5)) (:raw 1)))))))

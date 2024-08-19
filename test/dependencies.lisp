(cl:in-package #:incrementalist.test)

(def-suite* :incrementalist.dependencies
  :in :incrementalist)

(test dependencies.within-expression.1
  (edits-cases ()
    ("(foo

 bar)"
     `((inc:cons-wad ((0 0) (2 5)) ()
        ,(expected-symbol-wad '((0 1) (0 4)) "FOO")
        ,(expected-symbol-wad '((2 1) (2 4)) "BAR")))
     '(:insert (1 0) " (cl:in-package #:BAZ)")
     `((inc:cons-wad ((0 0) (2 5)) ()
        ,(expected-symbol-wad '((0 1) (0 4)) "FOO")
        (inc:cons-wad ((1 1) (1 22)) ()
         ,(expected-symbol-wad
           '((1 2) (1 15)) "IN-PACKAGE"
           :token-class  'incrementalist:existing-symbol-token
           :package-name "COMMON-LISP")
         ,(expected-symbol-wad
           '((1 16) (1 21)) "BAZ"
           :token-class  'incrementalist:uninterned-symbol-token
           :package-name nil
           :words        '(:ignore-children)))
        ,(expected-symbol-wad '((2 1) (2 4)) "BAR")))
     '(:erase (1 1) 21)
     `((inc:cons-wad ((0 0) (2 5)) ()
        ,(expected-symbol-wad '((0 1) (0 4)) "FOO")
        ,(expected-symbol-wad '((2 1) (2 4)) "BAR"))))))

#-ccl ; CCL doesn't allow the way we mix labeled objects and quasiquote
(test dependencies.within-expression.2
  ;; As sub-expressions of a single toplevel expression, a form that
  ;; sets `*read-base*' at read-time between other expressions.
  (edits-cases ()
    ("(10

 10)"
     '((inc:cons-wad ((0 0) (2 4)) ()
        (inc:atom-wad ((0 1) (0 3)) (:raw 10))
        (inc:atom-wad ((2 1) (2 3)) (:raw 10))))
     ;; At read-time, set `*read-base*' to 16
     '(:insert (1 0) " #.(cl:setf cl:*read-base* 16)")
     `((inc:cons-wad ((0 0) (2 4)) ()
        (inc:atom-wad ((0 1) (0 3)) (:raw 10))
        (inc:atom-wad ((1 1) (1 30)) (:raw 16)
         (inc:cons-wad ((1 3) (1 30)) ()
          ,#1=(expected-symbol-wad '((1 4) (1 11)) "SETF"
               :token-class  'incrementalist:existing-symbol-token
               :package-name "COMMON-LISP")
          ,#2=(expected-symbol-wad '((1 12) (1 26)) "*READ-BASE*"
               :token-class  'incrementalist:existing-symbol-token
               :package-name "COMMON-LISP")
          (inc:atom-wad ((1 27) (1 29)) (:raw 16))))
        (inc:atom-wad ((2 1) (2 3)) (:raw 16))))
     ;; Set `*read-base*' to 17 instead of 16.
     '(:replace (1 27) "17")
     `((inc:cons-wad ((0 0) (2 4)) ()
        (inc:atom-wad ((0 1) (0 3)) (:raw 10))
        #3=(inc:atom-wad ((1 1) (1 30)) (:raw 17)
         (inc:cons-wad ((1 3) (1 30)) ()
          ,#1#
          ,#2#
          (inc:atom-wad ((1 27) (1 29)) (:raw 17))))
        (inc:atom-wad ((2 1) (2 3)) (:raw 17))))
     ;; Change the first child from 10 to 11
     '(:replace (0 1) "11")
     `((inc:cons-wad ((0 0) (2 4)) ()
       (inc:atom-wad ((0 1) (0 3)) (:raw 11))
       #3#
       (inc:atom-wad ((2 1) (2 3)) (:raw 17))))
     ;; Change the third child from 10 to 11
     '(:replace (2 1) "11")
     `((inc:cons-wad ((0 0) (2 4)) ()
       (inc:atom-wad ((0 1) (0 3)) (:raw 11))
       #3#
       (inc:atom-wad ((2 1) (2 3)) (:raw 18))))
     ;; Remove form which sets `*read-base*'
     '(:erase (1 1) 29)
     '((inc:cons-wad ((0 0) (2 4)) ()
        (inc:atom-wad ((0 1) (0 3)) (:raw 11))
        (inc:atom-wad ((2 1) (2 3)) (:raw 11)))))))

(test dependencies.within-expression.3
  ;; Insert and delete a labeled object definition and a reference
  ;; within the same expression.
  (edits-cases ()
    ("(progn

 #1#)"
     `((inc:cons-wad ((0 0) (2 5)) ()
        ,(expected-symbol-wad '((0 1) (0 6)) "PROGN")
        (inc:atom-wad ((2 1) (2 4))
         (:errors ((((2 2) (2 3))
                    eclector.reader:sharpsign-sharpsign-undefined-label))))))
     '(:insert (1 0) " #1=foo")
     `((inc:cons-wad ((0 0) (2 5)) ()
        ,(expected-symbol-wad '((0 1) (0 6)) "PROGN")
        (inc:labeled-object-definition-wad ((1 1) (1 7)) ()
         ,(expected-symbol-wad '((1 4) (1 7)) "FOO"))
        (inc:labeled-object-reference-wad ((2 1) (2 4)) ())))
     '(:insert (2 2) "0")
     `((inc:cons-wad ((0 0) (2 6)) ()
        ,(expected-symbol-wad '((0 1) (0 6)) "PROGN")
        (inc:labeled-object-definition-wad ((1 1) (1 7)) ()
         ,(expected-symbol-wad '((1 4) (1 7)) "FOO"))
        (inc:labeled-object-reference-wad ((2 1) (2 5)) ())))
     '(:erase (1 0) 7)
     `((inc:cons-wad ((0 0) (2 6)) ()
        ,(expected-symbol-wad '((0 1) (0 6)) "PROGN")
        (inc:atom-wad ((2 1) (2 5))
         (:errors ((((2 3) (2 4))
          eclector.reader:sharpsign-sharpsign-undefined-label))))))
     '(:insert (1 0) " (progn #1=foo)")
     `((inc:cons-wad ((0 0) (2 6)) ()
       ,(expected-symbol-wad '((0 1) (0 6)) "PROGN")
       (inc:cons-wad ((1 1) (1 15)) ()
        ,(expected-symbol-wad '((1 2) (1 7)) "PROGN")
        (inc:labeled-object-definition-wad ((1 8) (1 14)) ()
        ,(expected-symbol-wad '((1 11) (1 14)) "FOO")))
       (inc:labeled-object-reference-wad ((2 1) (2 5)) ())))
     '(:erase (2 2) 1)
     `((inc:cons-wad ((0 0) (2 5)) ()
        ,(expected-symbol-wad '((0 1) (0 6)) "PROGN")
        (inc:cons-wad ((1 1) (1 15)) ()
         ,(expected-symbol-wad '((1 2) (1 7)) "PROGN")
         (inc:labeled-object-definition-wad ((1 8) (1 14)) ()
          ,(expected-symbol-wad '((1 11) (1 14)) "FOO")))
        (inc:labeled-object-reference-wad ((2 1) (2 4)) ()))))))

(test dependencies.within-expressions.4
  "Splitting and joining expressions must propagate labeled objects aspect
correctly."
  (edits-cases ()
    ("(#1=1 #2=2

 #1=1 #2=2)"
     #1=`((inc:cons-wad ((0 0) (2 11)) ()
        #2=(inc:labeled-object-definition-wad ((0 1) (0 5)) ()
            (inc:atom-wad ((0 4) (0 5)) (:raw 1)))
        #3=(inc:labeled-object-definition-wad ((0 6) (0 10)) ()
            (inc:atom-wad ((0 9) (0 10)) (:raw 2)))
        (inc:atom-wad ((2 1) (2 5))
           (:raw    1
            :errors ((((2 2) (2 3))
                      eclector.reader:sharpsign-equals-label-defined-more-than-once)))
         (inc:atom-wad ((2 4) (2 5)) (:raw 1)))
        (inc:atom-wad ((2 6) (2 10))
           (:raw    2
            :errors ((((2 7) (2 8))
                      eclector.reader:sharpsign-equals-label-defined-more-than-once)))
         (inc:atom-wad ((2 9) (2 10)) (:raw 2)))))
     '(:insert (1 0) ")(")
     `((inc:cons-wad ((0 0) (1 1)) () #2# #3#)
       (inc:cons-wad ((1 1) (2 11)) ()
        (inc:labeled-object-definition-wad ((2 1) (2 5)) ()
         (inc:atom-wad ((2 4) (2 5)) (:raw 1)))
        (inc:labeled-object-definition-wad ((2 6) (2 10)) ()
         (inc:atom-wad ((2 9) (2 10)) (:raw 2)))))
     '(:erase (1 0) 2)
     #1#)))

(test dependencies.between-expressions.1
  (edits-cases ()
    ("foo

bar"
     `(,(expected-symbol-wad '((0 0) (0 3)) "FOO")
       ,(expected-symbol-wad '((2 0) (2 3)) "BAR"))
     '(:insert (1 0) "(cl:in-package #:BAZ)")
     `(,(expected-symbol-wad '((0 0) (0 3)) "FOO")
       (inc:cons-wad ((1 0) (1 21)) ()
        ,(expected-symbol-wad '((1 1) (1 14)) "IN-PACKAGE"
                              :token-class  'inc:existing-symbol-token
                              :package-name "COMMON-LISP")
        ,(expected-symbol-wad '((1 15) (1 20)) "BAZ"
                              :token-class  'inc:uninterned-symbol-token
                              :package-name nil
                              :words        '(:ignore-children)))
       ,(expected-symbol-wad
         '((2 0) (2 3)) "BAR"
         :token-class  'inc:non-existing-package-symbol-token
         :package-name "BAZ"))
     '(:erase (1 0) 21)
     `(,(expected-symbol-wad '((0 0) (0 3)) "FOO")
       ,(expected-symbol-wad '((2 0) (2 3)) "BAR")))))

#-ccl ; CCL doesn't allow the way we mix labeled objects and quasiquote
(test dependencies.between-expressions.2
  ;; Two toplevel expressions, a form that sets `*read-base*' at
  ;; read-time between other expressions.
  (edits-cases ()
    ("10

10"
     '((inc:atom-wad ((0 0) (0 2)) (:raw 10))
       (inc:atom-wad ((2 0) (2 2)) (:raw 10)))
     ;; At read-time, set `*read-base*' to 16
     '(:insert (1 0) "#.(cl:setf cl:*read-base* 16)")
     `((inc:atom-wad ((0 0) (0 2)) (:raw 10))
       (inc:atom-wad ((1 0) (1 29)) (:raw 16)
        (inc:cons-wad ((1 2) (1 29)) ()
         ,#1=(expected-symbol-wad
              '((1 3) (1 10)) "SETF"
              :token-class  'incrementalist:existing-symbol-token
              :package-name "COMMON-LISP")
         ,#2=(expected-symbol-wad
              '((1 11) (1 25)) "*READ-BASE*"
              :token-class  'incrementalist:existing-symbol-token
              :package-name "COMMON-LISP")
         (inc:atom-wad ((1 26) (1 28)) (:raw 16))))
       (inc:atom-wad ((2 0) (2 2)) (:raw 16)))
     ;; Set `*read-base*' to 17 instead of 16.
     '(:replace (1 26) "17")
     `((inc:atom-wad ((0 0) (0 2)) (:raw 10))
       #3=(inc:atom-wad ((1 0) (1 29)) ()
           (inc:cons-wad ((1 2) (1 29)) ()
            ,#1#
            ,#2#
            (inc:atom-wad ((1 26) (1 28)) (:raw 17))))
       (inc:atom-wad ((2 0) (2 2)) (:raw 17)))
     ;; Change the first child from 10 to 11
     '(:replace (0 0) "11")
     `((inc:atom-wad ((0 0) (0 2)) (:raw 11))
       #3#
       (inc:atom-wad ((2 0) (2 2)) (:raw 17)))
     ;; Change the third child from 10 to 11
     '(:replace (2 0) "11")
     `((inc:atom-wad ((0 0) (0 2)) (:raw 11))
       #3#
       (inc:atom-wad ((2 0) (2 2)) (:raw 18)))
     ;; Remove form which sets `*read-base*'
     '(:erase (1 0) 29)
     '((inc:atom-wad ((0 0) (0 2)) (:raw 11))
       (inc:atom-wad ((2 0) (2 2)) (:raw 11))))))

#-ccl ; CCL doesn't allow the way we mix labeled objects and quasiquote
(test dependencies.between-expressions.3
  (edits-cases ()
    ;;
    ("
#.(cl:setf cl:*read-base* 10)
foo"
     `(#1=(inc:atom-wad ((1 0) (1 29)) (:raw 10)
        (inc:cons-wad ((1 2) (1 29)) ()
         ,(expected-symbol-wad '((1 3) (1 10)) "SETF"
                               :token-class  'inc:existing-symbol-token
                               :package-name "COMMON-LISP")
         ,(expected-symbol-wad '((1 11) (1 25)) "*READ-BASE*"
                               :token-class  'inc:existing-symbol-token
                               :package-name "COMMON-LISP")
         (inc:atom-wad ((1 26) (1 28)) (:raw 10))))
       ,(expected-symbol-wad '((2 0) (2 3)) "FOO"))
     ;; Change the current package.
     '(:insert (0 0) "(cl:in-package #:foo)")
     `((inc:cons-wad ((0 0) (0 21)) ()
        ,(expected-symbol-wad '((0 1) (0 14)) "IN-PACKAGE"
                              :token-class  'inc:existing-symbol-token
                              :package-name "COMMON-LISP")
        ,(expected-symbol-wad '((0 15) (0 20)) "FOO"
                              :token-class  'inc:uninterned-symbol-token
                              :package-name nil
                              :words        '(:ignore-children)))
       #1#
       ,(expected-symbol-wad
         '((2 0) (2 3)) "FOO"
         :token-class  'inc:non-existing-package-symbol-token
         :package-name "FOO")))))

(test dependencies.between-expressions.4
  (edits-cases ()
    ;; A string followed by a symbol.
    ("\"
foo
\"
bar
"
     `((inc:atom-wad ((0 0) (2 1)) (:raw #.(format nil "~%foo~%"))
        (inc:word-wad ((1 0) (1 3))))
       ,(expected-symbol-wad '((3 0) (3 3)) "BAR"))
     ;; Turn line 1 into a symbol and lines 2 to 4 into a string.
     '(progn
       (:erase (0 0) 1)
       (:insert (4 0) "\""))
     `(,(expected-symbol-wad '((1 0) (1 3)) "FOO")
       (inc:atom-wad ((2 0) (4 1)) (:raw #.(format nil "~%bar~%"))
        (inc:word-wad ((3 0) (3 3))))))))

(test dependencies.between-expressions.5
  (edits-cases ()
    ("
#+foo
#-bar baz
111"
     #1=`((inc:skipped-positive-conditional-wad ((1 0) (2 9)) ()
           ,(expected-symbol-wad '((1 2) (1 5)) "FOO"
                                 :token-class  'inc:existing-symbol-token
                                 :package-name "KEYWORD")
           (inc:read-suppress-wad ((2 0) (2 9)) ()
            ,(expected-symbol-wad '((2 2) (2 5)) "BAR"
                                  :token-class  'inc:existing-symbol-token
                                  :package-name "KEYWORD")
         (inc:read-suppress-wad ((2 6) (2 9)) ())))
       (inc:atom-wad ((3 0) (3 3)) (:raw 111)))
     ;; Add reader conditional before everything
     '(:insert (0 0) "#+()")
     `((inc:skipped-positive-conditional-wad ((0 0) (3 3)) ()
        ,(expected-symbol-wad '((0 2) (0 4)) "NIL"
                              :token-class  'inc:existing-symbol-token
                              :package-name "COMMON-LISP")
        (inc:skipped-positive-conditional-wad ((1 0) (2 9)) ()
         ,(expected-symbol-wad '((1 2) (1 5)) "FOO"
                               :token-class  'inc:existing-symbol-token
                               :package-name "KEYWORD")
         (inc:read-suppress-wad ((2 0) (2 9)) ()
          ,(expected-symbol-wad '((2 2) (2 5)) "BAR"
                                :token-class  'inc:existing-symbol-token
                                :package-name "KEYWORD")
          (inc:read-suppress-wad ((2 6) (2 9)) ())))
        (inc:read-suppress-wad ((3 0) (3 3)))))
     ;; Delete the initial reader conditional again
     '(:erase (0 0) 4)
     #1#)))

(test dependencies.between-expressions.6
  (edits-cases ()
    ("#+()
#+()
1"
     `((inc:skipped-positive-conditional-wad ((0 0) (2 1))
          (:errors ((((2 1) (2 1)) eclector.reader:end-of-file)))
        ,(expected-symbol-wad '((0 2) (0 4)) "NIL"
                              :token-class  'inc:existing-symbol-token
                              :package-name "COMMON-LISP")
        (inc:skipped-positive-conditional-wad ((1 0) (2 1)) ()
         ,(expected-symbol-wad '((1 2) (1 4)) "NIL"
                               :token-class  'inc:existing-symbol-token
                               :package-name "COMMON-LISP")
         (inc:read-suppress-wad ((2 0) (2 1))))))
     '(:erase (0 0) 4)
     `((inc:skipped-positive-conditional-wad ((1 0) (2 1)) ()
       ,(expected-symbol-wad '((1 2) (1 4)) "NIL"
                             :token-class  'inc:existing-symbol-token
                             :package-name "COMMON-LISP")
       (inc:read-suppress-wad ((2 0) (2 1))))))))

(test dependencies.between-expressions.7
  (edits-cases ()
    ("(cl:in-package #:common-lisp)
; (cl:in-package #:incrementalist.test.test-package)
;;;
defun"
     `((inc:cons-wad ((0 0) (0 29)) ()
        ,(expected-symbol-wad '((0 1) (0 14)) "IN-PACKAGE"
                              :token-class  'inc:existing-symbol-token
                              :package-name "COMMON-LISP")
        ,(expected-symbol-wad '((0 15) (0 28)) "COMMON-LISP"
                              :token-class  'inc:uninterned-symbol-token
                              :package-name nil
                              :words        '(:ignore-children)))
       (inc:semicolon-comment-wad ((1 0) (2 0)) () :ignore-children)
       (inc:semicolon-comment-wad ((2 0) (3 0)) () :ignore-children)
       ,(expected-symbol-wad '((3 0) (3 5)) "DEFUN"
                             :token-class  'inc:existing-symbol-token
                             :package-name "COMMON-LISP"))
     ;; Activate the second `in-package' form.
     '(:erase (1 0) 2)
     `((inc:cons-wad ((0 0) (0 29)) ()
        ,(expected-symbol-wad '((0 1) (0 14)) "IN-PACKAGE"
                              :token-class  'inc:existing-symbol-token
                              :package-name "COMMON-LISP")
        ,(expected-symbol-wad '((0 15) (0 28)) "COMMON-LISP"
                              :token-class  'inc:uninterned-symbol-token
                              :package-name nil
                              :words        '(:ignore-children)))
       (inc:cons-wad ((1 0) (1 50)) ()
        ,(expected-symbol-wad '((1 1) (1 14)) "IN-PACKAGE"
                              :token-class  'inc:existing-symbol-token
                              :package-name "COMMON-LISP")
        ,(expected-symbol-wad '((1 15) (1 49)) "INCREMENTALIST.TEST.TEST-PACKAGE"
                              :token-class  'inc:uninterned-symbol-token
                              :package-name nil
                              :words        '(:ignore-children)))
       (inc:semicolon-comment-wad ((2 0) (3 0)) () :ignore-children)
       ,(expected-symbol-wad '((3 0) (3 5)) "DEFUN"
                             :package-name "INCREMENTALIST.TEST.TEST-PACKAGE"
                             :words        '(:ignore-children))))))

(test dependencies.between-expressions.8
  "Labeled objects must not propagate between toplevel expressions."
  (edits-cases ()
    ("(#1=1 #2=2)
(#1=1 #2=2)"
     #1=`((inc:cons-wad ((0 0) (0 11)) ()
           (inc:labeled-object-definition-wad ((0 1) (0 5)) ()
            (inc:atom-wad ((0 4) (0 5)) (:raw 1)))
           (inc:labeled-object-definition-wad ((0 6) (0 10)) ()
            (inc:atom-wad ((0 9) (0 10)) (:raw 2))))
          (inc:cons-wad ((1 0) (1 11)) ()
           (inc:labeled-object-definition-wad ((1 1) (1 5)) ()
            (inc:atom-wad ((1 4) (1 5)) (:raw 1)))
           (inc:labeled-object-definition-wad ((1 6) (1 10)) ()
            (inc:atom-wad ((1 9) (1 10)) (:raw 2)))))
     ;; Force re-reading of the first expression.  The changes to the
     ;; labeled objects must not affect the second expression.
     '(:poke (0 0))
     #1#
     ;; Force re-reading of the second expression.
     '(:poke (1 0))
     #1#)))

(test dependencies.between-expressions.9
  "Changing whether a symbol is part of a feature expression has to
update the symbol package correctly."
  (edits-cases ()
    ("#+

foo
bar
"
     #1=`((inc:skipped-positive-conditional-wad ((0 0) (3 3)) ()
           ,(expected-symbol-wad '((2 0) (2 3)) "FOO"
                                 :token-class  'inc:existing-symbol-token
                                 :package-name "KEYWORD")
           (inc:read-suppress-wad ((3 0) (3 3)) ())))
     ;; Make both symbols part of the feature expression
     '(:insert (1 0) "(")
     `((inc:skipped-positive-conditional-wad ((0 0) (4 0))
          (:errors ((((4 0) (4 0))
                     eclector.reader:end-of-input-after-feature-expression)))
        (inc:cons-wad ((1 0) (4 0))
           (:errors ((((4 0) (4 0)) eclector.reader:unterminated-list)))
         ,(expected-symbol-wad '((2 0) (2 3)) "FOO"
                               :token-class  'inc:existing-symbol-token
                               :package-name "KEYWORD")
         ,(expected-symbol-wad '((3 0) (3 3)) "BAR"
                               :token-class  'inc:existing-symbol-token
                               :package-name "KEYWORD"))))
     ;; Restore previous state
     '(:replace (1 0) " ")
     #1#)))

(test dependencies.between-expressions.10
  "Ensure that invalidation works for inherited cells without users."
  (edits-cases ()
    ("(cl:in-package #:common-lisp-user)
10
11"
     #1='((inc:cons-wad ((0 0) (0 34)) () :ignore-children)
          (inc:atom-wad ((1 0) (1 2)) (:raw 10))
          (inc:atom-wad ((2 0) (2 2)) (:raw 11)))
     ;; Force an update of the `in-package' expression. This
     ;; invalidates the `*package*' cell that is inherited by the
     ;; following expressions which are on the suffix at that
     ;; time. Since no following expression explicitly uses the cell,
     ;; the required invalidation must take into account inherited
     ;; cells in toplevel wads in addition to explicit cell usage.
     '(:poke (0 0))
     #1#
     ;; In case the invalidation didn't work properly (which was the
     ;; case at one point), the state that is installed from the
     ;; middle expression will contain the invalid inherited
     ;; `*package*' cell.
     '(:poke (2 0))
     #1#)))

(test dependencies.between-expressions.11
  "Multiple cells escaping from a single expression."
  ;; The quoted expression on the first line has two escaping cells:
  ;; one for the labeled object definition and one for the read-time
  ;; evaluation.
  (edits-cases ()
    ("'(#1=20 #.(cl:setf cl:*read-base* 11))
10"
     `((inc:cons-wad ((0 0) (0 38)) ()
        (inc:cons-wad ((0 1) (0 38)) ()
         (inc:labeled-object-definition-wad ((0 2) (0 7)) ()
          (inc:atom-wad ((0 5) (0 7)) (:raw 20)))
         (inc:atom-wad ((0 8) (0 37)) (:raw 11)
          (inc:cons-wad ((0 10) (0 37)) ()
           ,(expected-symbol-wad '((0 11) (0 18)) "SETF"
                                 :token-class  'inc:existing-symbol-token
                                 :package-name "COMMON-LISP")
           ,(expected-symbol-wad '((0 19) (0 33)) "*READ-BASE*"
                                 :token-class  'inc:existing-symbol-token
                                 :package-name "COMMON-LISP")
           (inc:atom-wad ((0 34) (0 36)) (:raw 11))))))
       (inc:atom-wad ((1 0) (1 2)) (:raw 11))))
    ("'(#.(cl:setf cl:*read-base* 11) #1=20)
10"
     `((inc:cons-wad ((0 0) (0 38)) ()
        (inc:cons-wad ((0 1) (0 38)) ()
         (inc:atom-wad ((0 2) (0 31)) (:raw 11)
          (inc:cons-wad ((0 4) (0 31)) ()
           ,(expected-symbol-wad '((0 5) (0 12)) "SETF"
                                 :token-class  'inc:existing-symbol-token
                                 :package-name "COMMON-LISP")
           ,(expected-symbol-wad '((0 13) (0 27)) "*READ-BASE*"
                                 :token-class  'inc:existing-symbol-token
                                 :package-name "COMMON-LISP")
           (inc:atom-wad ((0 28) (0 30)) (:raw 11))))
         (inc:labeled-object-definition-wad ((0 32) (0 37)) ()
          (inc:atom-wad ((0 35) (0 37)) (:raw 22)))))
       (inc:atom-wad ((1 0) (1 2)) (:raw 11))))))

(test dependencies.dangling.1
  "Ensure absence of dangling user references in global cells."
  (insert-then-delete
   "(
(a .
`b))"
   :stream nil))


(test dependencies.dangling.2
  "Ensure absence of dangling user references in global cells."
  (insert-then-delete
   "(
(a
))"
   :stream nil))

(test dependencies.dangling.3
  "Ensure absence of dangling user references in global cells."
  (insert-then-delete
   "(\"a\"
'\"(b)\"
)"
   :stream nil))

(test dependencies.dangling.4
  "Ensure absence of dangling user references in global cells."
  (insert-then-delete "#+a b" :stream nil))

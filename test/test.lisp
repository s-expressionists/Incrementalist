(cl:in-package #:incrementalist.test)

(in-suite :incrementalist)

(test smoke
  "Initial smoke test for the incrementalist system."
  (analysis-cases ()
    ("()"
     `(,(expected-symbol-wad '((0 0) (0 2)) 'nil)))
    ("#\\Return"
     '((inc:atom-wad ((0 0) (0 8)) (:raw #\Return))))
    ("\"foo\""
     '((inc:atom-wad ((0 0) (0 5)) (:raw "foo")
        (inc:word-wad ((0 1) (0 4))))))
    ("\".\""
     '((inc:atom-wad ((0 0) (0 3)) (:raw ".")
        (inc:text-wad ((0 1) (0 2))))))
    ("||"
     '((inc:atom-wad ((0 0) (0 2))
          (:raw (inc:interned-symbol-token :symbol ("TEST" "")))
        (inc:punctuation-wad ((0 0) (0 1)))
        (inc:punctuation-wad ((0 1) (0 2))))))
    ("#:foo"
     `((inc:atom-wad ((0 0) (0 5))
          (:raw (inc:uninterned-symbol-token :symbol (nil "FOO")))
        (inc:punctuation-wad ((0 0) (0 1)))
        (inc:punctuation-wad ((0 1) (0 2)))
        (inc:word-wad ((0 2) (0 5))))))
    ("test::bar"
     '((inc:atom-wad ((0 0) (0 9))
          (:raw (inc:symbol-token :symbol ("TEST" "BAR")))
        (inc:word-wad ((0 0) (0 4)))
        (inc:punctuation-wad ((0 4) (0 5)))
        (inc:punctuation-wad ((0 5) (0 6)))
        (inc:word-wad ((0 6) (0 9))))))
    ("#'foo"
     `((inc:cons-wad ((0 0) (0 5)) ()
        ,(expected-symbol-wad '((0 2) (0 5)) "FOO"
                              :token-class 'inc:interned-symbol-token))))
    ("'(1 2 3)"
     '((inc:cons-wad ((0 0) (0 8)) ()
        (inc:cons-wad ((0 1) (0 8)) ()
         (inc:atom-wad ((0 2) (0 3)) (:raw 1))
         (inc:atom-wad ((0 4) (0 5)) (:raw 2))
         (inc:atom-wad ((0 6) (0 7)) (:raw 3))))))
    ("`(1 ,2 ,@(3))"
     '((inc:cons-wad ((0 0) (0 13)) ()
        (inc:cons-wad ((0 1) (0 13)) ()
         (inc:atom-wad ((0 2) (0 3)) (:raw 1))
         (inc:cons-wad ((0 4) (0 6)) ()
          (inc:atom-wad ((0 5) (0 6)) (:raw 2)))
         (inc:cons-wad ((0 7) (0 12)) ()
          (inc:cons-wad ((0 9) (0 12)) ()
           (inc:atom-wad ((0 10) (0 11)) (:raw 3))))))))
    ;; List with sharing but no circularity. One reference and two
    ;; references.
    ("(#1=1 #1# 3)"
     '((inc:cons-wad ((0 0) (0 12)) ()
        (inc:labeled-object-definition-wad ((0 1) (0 5)) ()
         (inc:atom-wad ((0 4) (0 5)) (:raw 1)))
        (inc:labeled-object-reference-wad ((0 6) (0 9)) (:raw 1))
        (inc:atom-wad ((0 10) (0 11)) (:raw 3)))))
    ("(#2=1 #2# #2#)"
     '((inc:cons-wad ((0 0) (0 14)) ()
        (inc:labeled-object-definition-wad ((0 1) (0 5)) ()
         (inc:atom-wad ((0 4) (0 5)) (:raw 1)))
        (inc:labeled-object-reference-wad ((0 6) (0 9)) (:raw 1))
        (inc:labeled-object-reference-wad ((0 10) (0 13)) (:raw 1)))))
    ;; List with circularity. One reference and two references.
    ("#1=(1 #1# 3)"
     '((inc:labeled-object-definition-wad ((0 0) (0 12)) ()
        (inc:cons-wad ((0 3) (0 12)) ()
         (inc:atom-wad ((0 4) (0 5)) (:raw 1))
         (inc:labeled-object-reference-wad ((0 6) (0 9)) ())
         (inc:atom-wad ((0 10) (0 11)) (:raw 3))))))
    ("#10=(1 #10# #10#)"
     '((inc:labeled-object-definition-wad ((0 0) (0 17)) ()
        (inc:cons-wad ((0 4) (0 17)) ()
         (inc:atom-wad ((0 5) (0 6)) (:raw 1))
         (inc:labeled-object-reference-wad ((0 7) (0 11)) ())
         (inc:labeled-object-reference-wad ((0 12) (0 16)) ())))))
    ;; Vector with sharing but no circularity. One reference and two
    ;; references.
    ("#(#1=1 #1# 3)"
     '((inc:atom-wad ((0 0) (0 13)) ()
        (inc:labeled-object-definition-wad ((0 2) (0 6)) ()
         (inc:atom-wad ((0 5) (0 6)) (:raw 1)))
        (inc:labeled-object-reference-wad ((0 7) (0 10)) (:raw 1))
        (inc:atom-wad ((0 11) (0 12)) (:raw 3)))))
    ("#(#1=1 #1# #1#)"
     '((inc:atom-wad ((0 0) (0 15)) ()
        (inc:labeled-object-definition-wad ((0 2) (0 6)) ()
         (inc:atom-wad ((0 5) (0 6)) (:raw 1)))
        (inc:labeled-object-reference-wad ((0 7) (0 10)) (:raw 1))
        (inc:labeled-object-reference-wad ((0 11) (0 14)) (:raw 1)))))
    ;; Vector with circularity. One reference and two references.
    ("#1=#(1 #1# 3)"
     '((inc:labeled-object-definition-wad ((0 0) (0 13)) ()
        (inc:atom-wad ((0 3) (0 13)) ()
         (inc:atom-wad ((0 5) (0 6)) (:raw 1))
         (inc:labeled-object-reference-wad ((0 7) (0 10)) ())
         (inc:atom-wad ((0 11) (0 12)) (:raw 3))))))
    ("#1=#(1 #1# #1#)"
     '((inc:labeled-object-definition-wad ((0 0) (0 15)) ()
        (inc:atom-wad ((0 3) (0 15)) ()
         (inc:atom-wad ((0 5) (0 6)) (:raw 1))
         (inc:labeled-object-reference-wad ((0 7) (0 10)) ())
         (inc:labeled-object-reference-wad ((0 11) (0 14)) ())))))
    ;; Skipped input within labeled object definitions.
    ("#1=#|a|#1"
     '((inc:labeled-object-definition-wad ((0 0) (0 9)) ()
        (inc:block-comment-wad ((0 3) (0 8)) ()
         (inc:word-wad ((0 5) (0 6))))
        (inc:atom-wad ((0 8) (0 9)) (:raw 1)))))
    ("#1=(#|a|# #1#)"
     '((inc:labeled-object-definition-wad ((0 0) (0 14)) ()
        (inc:cons-wad ((0 3) (0 14)) ()
         (inc:block-comment-wad ((0 4) (0 9)) ()
          (inc:word-wad ((0 6) (0 7))))
         (inc:labeled-object-reference-wad ((0 10) (0 13)) ())))))
    ("#+common-lisp 1"
     `((inc:read-positive-conditional-wad ((0 0) (0 15))
          (:feature-expression (inc:interned-symbol-token
                                :symbol ("KEYWORD" "COMMON-LISP")))
        ,(expected-symbol-wad '((0 2) (0 13)) :common-lisp
                              :words '(:ignore-children))
        (inc:atom-wad ((0 14) (0 15)) (:raw 1)))))
    ("#+a b"
     `((inc:skipped-positive-conditional-wad ((0 0) (0 5))
          (:feature-expression (inc:interned-symbol-token :symbol ("KEYWORD" "A")))
        ,(expected-symbol-wad '((0 2) (0 3)) :a)
       (inc:read-suppress-wad ((0 4) (0 5)) ()))))
    ("#-a b"
     `((inc:read-negative-conditional-wad ((0 0) (0 5))
          (:raw (inc:interned-symbol-token :symbol ("TEST" "B")))
        ,(expected-symbol-wad '((0 2) (0 3)) :a)
        ,(expected-symbol-wad '((0 4) (0 5)) "B"))))
    ("#-
a b"
     `((inc:read-negative-conditional-wad ((0 0) (1 3)) ()
        ,(expected-symbol-wad '((1 0) (1 1)) :a)
        ,(expected-symbol-wad '((1 2) (1 3)) "B"))))
    ;; Read in a different package that uses the CL package
    ('("floor" :initial-package "INCREMENTALIST.TEST.USING-PACKAGE")
     `(,(expected-symbol-wad '((0 0) (0 5)) 'floor)))))

(test nil-spellings
  "Test for reading various spellings of `nil'."
  (analysis-cases ()
    ("()"
     `(,(expected-symbol-wad '((0 0) (0 2)) 'nil)))
    ('("nil" :initial-package "INCREMENTALIST.TEST.USING-PACKAGE")
     `(,(expected-symbol-wad '((0 0) (0 3)) 'nil)))
    ("cl:nil"
     `(,(expected-symbol-wad '((0 0) (0 6)) 'nil)))
    ("cl::nil"
     `(,(expected-symbol-wad '((0 0) (0 7)) 'nil)))
    ("common-lisp:nil"
     `(,(expected-symbol-wad '((0 0) (0 15)) 'nil)))
    ("common-lisp::nil"
     `(,(expected-symbol-wad '((0 0) (0 16)) 'nil)))))

(test incomplete
  "Tests for incomplete expressions."
  (analysis-cases ()
    ("(a #||#)"
     `((inc:cons-wad ((0 0) (0 8)) ()
        ,(expected-symbol-wad '((0 1) (0 2)) "A")
        (inc:block-comment-wad ((0 3) (0 7))))))
    ("((a . b) . ; foo"
     `((inc:cons-wad ((0 0) (0 16))
        (:errors ((((0 16) (0 16)) eclector.reader:end-of-input-after-consing-dot)
                  (((0 16) (0 16)) eclector.reader:unterminated-list)))
        (inc:cons-wad ((0 1) (0 8)) ()
         ,(expected-symbol-wad '((0 2) (0 3)) "A")
         ,(expected-consing-dot-wad '((0 4) (0 5)))
         ,(expected-symbol-wad '((0 6) (0 7)) "B"))
        ,(expected-consing-dot-wad '((0 9) (0 10)))
        ,(expected-semicolon-comment-wad '((0 11) (0 16))))))
    ("((a . b) . (c ."
     `((inc:cons-wad ((0 0) (0 15))
        (:errors ((((0 15) (0 15)) eclector.reader:unterminated-list)))
        (inc:cons-wad ((0 1) (0 8)) ()
         ,(expected-symbol-wad '((0 2) (0 3)) "A")
         ,(expected-consing-dot-wad '((0 4) (0 5)))
         ,(expected-symbol-wad '((0 6) (0 7)) "B"))
        ,(expected-consing-dot-wad '((0 9) (0 10)))
        (inc:cons-wad ((0 11) (0 15))
         (:errors ((((0 15) (0 15)) eclector.reader:end-of-input-after-consing-dot)
                   (((0 15) (0 15)) eclector.reader:unterminated-list)))
         ,(expected-symbol-wad '((0 12) (0 13)) "C")
         ,(expected-consing-dot-wad '((0 14) (0 15)))))))
    ("((a . b) . (c . d))"
     `((inc:cons-wad ((0 0) (0 19)) ()
        (inc:cons-wad ((0 1) (0 8)) ()
         ,(expected-symbol-wad '((0 2) (0 3)) "A")
         ,(expected-consing-dot-wad '((0 4) (0 5)))
         ,(expected-symbol-wad '((0 6) (0 7)) "B"))
        ,(expected-consing-dot-wad '((0 9) (0 10)))
        (inc:cons-wad ((0 11) (0 18)) ()
         ,(expected-symbol-wad '((0 12) (0 13)) "C")
         ,(expected-consing-dot-wad '((0 14) (0 15)))
         ,(expected-symbol-wad '((0 16) (0 17)) "D")))))
    ("((a . b) . (c . d) #||#)"
     `((inc:cons-wad ((0 0) (0 24)) ()
        (inc:cons-wad ((0 1) (0 8)) ()
         ,(expected-symbol-wad '((0 2) (0 3)) "A")
         ,(expected-consing-dot-wad '((0 4) (0 5)))
         ,(expected-symbol-wad '((0 6) (0 7)) "B"))
        ,(expected-consing-dot-wad '((0 9) (0 10)))
        (inc:cons-wad ((0 11) (0 18)) ()
         ,(expected-symbol-wad '((0 12) (0 13)) "C")
         ,(expected-consing-dot-wad '((0 14) (0 15)))
         ,(expected-symbol-wad '((0 16) (0 17)) "D"))
        (inc:block-comment-wad ((0 19) (0 23))))))
    ("((a . b) c . d #||#)"
     `((inc:cons-wad ((0 0) (0 20)) ()
       (inc:cons-wad ((0 1) (0 8)) ()
        ,(expected-symbol-wad '((0 2) (0 3)) "A")
        ,(expected-consing-dot-wad '((0 4) (0 5)))
        ,(expected-symbol-wad '((0 6) (0 7)) "B"))
       ,(expected-symbol-wad '((0 9) (0 10)) "C")
       ,(expected-consing-dot-wad '((0 11) (0 12)))
       ,(expected-symbol-wad '((0 13) (0 14)) "D")
       (inc:block-comment-wad ((0 15) (0 19))))))
    ("((a . b) #||# #||# . (c . d) #||#)"
     `((inc:cons-wad ((0 0) (0 34)) ()
       (inc:cons-wad ((0 1) (0 8)) ()
        ,(expected-symbol-wad '((0 2) (0 3)) "A")
        ,(expected-consing-dot-wad '((0 4) (0 5)))
        ,(expected-symbol-wad '((0 6) (0 7)) "B"))
       (inc:block-comment-wad ((0 9) (0 13)))
       (inc:block-comment-wad ((0 14) (0 18)))
       ,(expected-consing-dot-wad '((0 19) (0 20)))
       (inc:cons-wad ((0 21) (0 28)) ()
        ,(expected-symbol-wad '((0 22) (0 23)) "C")
        ,(expected-consing-dot-wad '((0 24) (0 25)))
        ,(expected-symbol-wad '((0 26) (0 27)) "D"))
       (inc:block-comment-wad ((0 29) (0 33))))))
    ("`(a . (b ,c))"
     `((inc:cons-wad ((0 0) (0 13)) ()
        (inc:cons-wad ((0 1) (0 13)) ()
         ,(expected-symbol-wad '((0 2) (0 3)) "A")
         ,(expected-consing-dot-wad '((0 4) (0 5)))
         (inc:cons-wad ((0 6) (0 12)) ()
          ,(expected-symbol-wad '((0 7) (0 8)) "B")
         (inc:cons-wad ((0 9) (0 11)) ()
          ,(expected-symbol-wad '((0 10) (0 11)) "C")))))))))

(test errors
  "Tests for errors."
  (analysis-cases ()
    ("."
     '((inc:reader-macro-wad ((0 0) (0 1))
        (:errors ((((0 0) (0 1)) eclector.reader:invalid-context-for-consing-dot))))))
    ("#\\does-not-exist"
     '((inc:atom-wad ((0 0) (0 16))
        (:errors ((((0 2) (0 16)) eclector.reader:unknown-character-name))))))
    ("#b00200"
     '((inc:atom-wad ((0 0) (0 7))
        (:errors ((((0 4) (0 5)) eclector.reader:digit-expected))))))
    ("("
     '((inc:atom-wad ((0 0) (0 1))
        (:errors ((((0 1) (0 1)) eclector.reader:unterminated-list))))))
    ;; Make sure that errors can be attached to results of kind :SKIP.
    ;; This can happen when the reader encounters unknown macro
    ;; (sub-)characters as in the following test case.
    ("#{"
     '((inc:reader-macro-wad ((0 0) (0 2))
        (:errors ((((0 1) (0 2)) eclector.reader:unknown-macro-sub-character))))))
    ;; An error from a read-time evaluation.  At the moment, the error
    ;; is caused by the fact that our read-time evaluation is too
    ;; limited but that still works for simulating an error.
    ("#.(cl:setf cl:*read-base* (cl:error \"foo\"))"
     '((inc:atom-wad ((0 0) (0 43))
        (:errors ((((0 42) (0 43)) eclector.reader:read-time-evaluation-error)))
        :ignore-children)))
    ;; Eclector errors can cross line boundaries: Eclector reads #\#
    ;; and #\Newline, then signals an error with offset -1 and length
    ;; 1 to indicate that the error is at the invalid dispatch
    ;; character #\Newline.  Ensure that the `error-wad' derived from
    ;; the Eclector error spans the two lines in order to indicate the
    ;; character.
    ("#
"
     '((inc:atom-wad ((0 0) (1 0))
        (:errors ((((0 1) (1 0)) eclector.reader:sharpsign-invalid)))
        :ignore-children)))
    ;; Eclector used to not report a consing dot in an invalid context
    ;; as skipped material after recovering form the error.  Make sure
    ;; the skipped material is represented (as a `reader-macro-wad')
    ;; and that the error is attached correctly.
    ("(. 1)"
     '((inc:cons-wad ((0 0) (0 5)) ()
        (inc:reader-macro-wad ((0 1) (0 2))
         (:errors ((((0 1) (0 2)) eclector.reader:invalid-context-for-consing-dot))))
        (inc:atom-wad ((0 3) (0 4)) (:raw 1)))))
    ("#1=."
     '((inc:atom-wad ((0 0) (0 4))
          (:errors ((((0 4) (0 4)) eclector.reader:end-of-input-after-sharpsign-equals)))
        (inc:reader-macro-wad ((0 3) (0 4))
         (:errors ((((0 3) (0 4)) eclector.reader:invalid-context-for-consing-dot)))))))
    ;; There used to be a problem with inherited symbols being
    ;; processed like (re)exported symbols.
    ("incrementalist.test.using-package:floor"
     '((inc:atom-wad ((0 0) (0 39))
        (:errors ((((0 34) (0 39)) eclector.reader:symbol-is-not-external))))))))

(test spell-checking.smoke
  "Test for spell-checking in symbols, strings and comments."
  (analysis-cases ()
    (";foo"
     '((inc:semicolon-comment-wad ((0 0) (0 4)) ()
        (inc:word-wad ((0 1) (0 4)) (:misspelled t)))))
    (";bar"
     '((inc:semicolon-comment-wad ((0 0) (0 4)) ()
        (inc:word-wad ((0 1) (0 4)) (:misspelled nil)))))
    ("#|foo|#"
     '((inc:block-comment-wad ((0 0) (0 7)) ()
        (inc:word-wad ((0 2) (0 5)) (:misspelled t)))))
    ("#|bar|#"
     '((inc:block-comment-wad ((0 0) (0 7)) ()
        (inc:word-wad ((0 2) (0 5)) (:misspelled nil)))))
    ("\"foo\""
     '((inc:atom-wad ((0 0) (0 5)) (:raw "foo")
        (inc:word-wad ((0 1) (0 4)) (:misspelled t)))))
    ("\"bar\""
     '((inc:atom-wad ((0 0) (0 5)) (:raw "bar")
        (inc:word-wad ((0 1) (0 4)) (:misspelled nil)))))
    ("foo"
     '((inc:atom-wad ((0 0) (0 3))
          (:raw (inc:interned-symbol-token :symbol ("TEST" "FOO")))
        (inc:word-wad ((0 0) (0 3)) (:misspelled t)))))
    ("bar"
     '((inc:atom-wad ((0 0) (0 3))
        (:raw (inc:interned-symbol-token :symbol ("TEST" "BAR")))
        (inc:word-wad ((0 0) (0 3)) (:misspelled nil)))))))

(cl:in-package #:incrementalist.test)

(in-suite :incrementalist)

(test smoke
  "Initial smoke test for the incrementalist system."
  (analysis-cases ()
    ("()"
     `(,(expected-symbol-wad '((0 0) (0 2)) "NIL"
                             :token-class  'inc:existing-symbol-token
                             :package-name "COMMON-LISP")))
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
          (:raw (inc:non-existing-symbol-token
                 :symbol ("INCREMENTALIST.TEST.TEST-PACKAGE" "")))
        (inc:punctuation-wad ((0 0) (0 1)))
        (inc:punctuation-wad ((0 1) (0 2))))))
    ("#:foo"
     `((inc:atom-wad ((0 0) (0 5))
          (:raw (inc:uninterned-symbol-token :symbol (nil "FOO")))
        (inc:punctuation-wad ((0 0) (0 1)))
        (inc:punctuation-wad ((0 1) (0 2)))
        (inc:word-wad ((0 2) (0 5))))))
    ("foo::bar"
     '((inc:atom-wad ((0 0) (0 8))
          (:raw (inc:symbol-token :symbol ("FOO" "BAR")))
        (inc:word-wad ((0 0) (0 3)))
        (inc:punctuation-wad ((0 3) (0 4)))
        (inc:punctuation-wad ((0 4) (0 5)))
        (inc:word-wad ((0 5) (0 8))))))
    ("#'foo"
     `((inc:cons-wad ((0 0) (0 5)) ()
        ,(expected-symbol-wad '((0 2) (0 5)) "FOO"
                              :token-class 'inc:non-existing-symbol-token))))
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
          (:feature-expression (inc:existing-symbol-token
                                :symbol ("KEYWORD" "COMMON-LISP")))
        ,(expected-symbol-wad '((0 2) (0 13)) "COMMON-LISP"
                              :token-class  'inc:existing-symbol-token
                              :package-name "KEYWORD"
                              :words        '(:ignore-children))
        (inc:atom-wad ((0 14) (0 15)) (:raw 1)))))
    ("#+a b"
     `((inc:skipped-positive-conditional-wad ((0 0) (0 5))
         (:feature-expression (inc:existing-symbol-token :symbol ("KEYWORD" "A")))
       ,(expected-symbol-wad '((0 2) (0 3)) "A"
                             :token-class  'inc:existing-symbol-token
                             :package-name "KEYWORD")
       (inc:read-suppress-wad ((0 4) (0 5)) ()))))
    ("#-a b"
     `((inc:read-negative-conditional-wad ((0 0) (0 5))
          (:raw (inc:non-existing-symbol-token :symbol ("INCREMENTALIST.TEST.TEST-PACKAGE" "B")))
        ,(expected-symbol-wad '((0 2) (0 3)) "A"
                              :token-class  'inc:existing-symbol-token
                              :package-name "KEYWORD")
        ,(expected-symbol-wad '((0 4) (0 5)) "B"))))
    ("#-
a b"
     `((inc:read-negative-conditional-wad ((0 0) (1 3)) ()
        ,(expected-symbol-wad '((1 0) (1 1)) "A"
                              :token-class  'inc:existing-symbol-token
                              :package-name "KEYWORD")
        ,(expected-symbol-wad '((1 2) (1 3)) "B"))))))

(test nil-spellings
  "Test for reading various spellings of `nil'."
  (analysis-cases ()
    ("()"
     '((inc:atom-wad ((0 0) (0 2)) (:raw (inc:existing-symbol-token
                                          :symbol ("COMMON-LISP" "NIL"))))))
    #+(or) ("nil" ; test package does not `:use' CL
            '((inc:atom-wad ((0 0) (0 3)) (:raw (inc:existing-symbol-token
                                                 :symbol ("COMMON-LISP" "NIL"))))))
    ("cl:nil"
     '((inc:atom-wad ((0 0) (0 6)) (:raw (inc:existing-symbol-token
                                          :symbol ("COMMON-LISP" "NIL"))))))
    ("cl::nil"
     '((inc:atom-wad ((0 0) (0 7)) (:raw (inc:existing-symbol-token
                                          :symbol ("COMMON-LISP" "NIL"))))))
    ("common-lisp:nil"
     '((inc:atom-wad ((0 0) (0 15)) (:raw (inc:existing-symbol-token
                                           :symbol ("COMMON-LISP" "NIL"))))))
    ("common-lisp::nil"
     '((inc:atom-wad ((0 0) (0 16)) (:raw (inc:existing-symbol-token
                                           :symbol ("COMMON-LISP" "NIL"))))))))

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
        (:errors ((((0 1) (0 2)) eclector.reader:unknown-macro-sub-character))))))))

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
          (:raw (inc:non-existing-symbol-token
                 :symbol ("INCREMENTALIST.TEST.TEST-PACKAGE" "FOO")))
        (inc:word-wad ((0 0) (0 3)) (:misspelled t)))))
    ("bar"
     '((inc:atom-wad ((0 0) (0 3))
        (:raw (inc:non-existing-symbol-token
               :symbol ("INCREMENTALIST.TEST.TEST-PACKAGE" "BAR")))
        (inc:word-wad ((0 0) (0 3)) (:misspelled nil)))))))

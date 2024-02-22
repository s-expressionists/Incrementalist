(cl:in-package #:incrementalist.test)

(in-suite :incrementalist)

(test smoke
  "Initial smoke test for the incrementalist system."
  (finishes (parse-result "foo::bar"))
  (finishes (parse-result "'(#1=1 #1# 3)"))
  (finishes (parse-result "'(1 2 3)")))

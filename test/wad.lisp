(cl:in-package #:incrementalist.test)

(def-suite* :incrementalist.wad
  :in :incrementalist)

(test items.smoke
  "Smoke test for the `items' function."
  (let ((result (parse-result "1")))
    (let ((first (first result)))
      (is (string= "1" (inc:items first)))))

  (let ((result (parse-result #.(format nil "(a~%)(~%b)"))))
    (let ((first (first result)))
      (is (string= #.(format nil "(a~%)") (inc:items first))))
    (let ((second (second result)))
      (is (string= #.(format nil "(~%b)") (inc:items second))))))

(defun state-value-equal (expected actual)
  (typecase expected
    (symbol (string= expected actual))
    (t      (equal expected actual))))

(test state-value.smoke
  "Smoke test for the `state-value' function."
  (labels ((get-wad (path result)
             (labels ((lookup (path node)
                        (if (null path)
                            node
                            (destructuring-bind (index &rest remaining) path
                              (typecase node
                                (cons
                                 (lookup remaining (nth index node)))
                                (t
                                 (let ((children (inc:children node)))
                                   (lookup remaining (elt children index)))))))))
               (lookup path result)))
           (check-wad (result path expected-state-values)
             (let ((wad (get-wad path result)))
               (loop :for (aspect . expected-value) :in expected-state-values
                     :for (actual-value actual-value-p)
                        = (multiple-value-list (inc:state-value wad aspect))
                     :do (case expected-value
                           (:missing
                            (is-false
                             actual-value-p
                             "~@<Expected no value of the aspect ~S to ~
                              present in wad ~A but a value is present.~@:>"
                             aspect wad))
                           (t
                            (is-true
                             actual-value-p
                             "~@<Expected a value of the aspect ~S to ~
                              present in wad ~A but no value is present.~@:>"
                             aspect wad)
                            (is (state-value-equal expected-value actual-value)
                                "~@<Expected the value of aspect ~S in wad ~A ~
                                to be ~S but it is ~S.~@:>"
                                aspect wad expected-value actual-value))))))
           (test-case (input specs)
             (loop :with result = (parse-result input)
                   :for (path expected-state-values) :in specs
                   :do (check-wad result path expected-state-values))))
    (test-case
     "1"
     '(((0) ((*package*       . "INCREMENTALIST.TEST.TEST-PACKAGE")
             (*read-suppress* . nil)
             (*read-base*     . 10)
             (foo             . :missing)))))
    (test-case
     "#-common-lisp 1"
     '(((0 0) ((*package*       . #:KEYWORD)
               (*read-suppress* . nil)
               (*read-base*     . 10)))
       ((0 1) ((*package*       . "INCREMENTALIST.TEST.TEST-PACKAGE")
               (*read-suppress* . t)
               (*read-base*     . 10)))))
    (test-case
     "#.(cl:setf cl:*read-base* 11) 1"
     '(((1) ((*package*       . "INCREMENTALIST.TEST.TEST-PACKAGE")
             (*read-suppress* . nil)
             (*read-base*     . 11)))))
    (test-case
     "(cl:in-package \"FOO\") 1"
     '(((1) ((*package*       . "FOO")
             (*read-suppress* . nil)
             (*read-base*     . 10)))))))

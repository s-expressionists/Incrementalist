(cl:in-package #:incrementalist.test)

(def-suite* :incrementalist.cache
  :in :incrementalist)

(defun is-query-result (expected-result actual-result &key input)
  (is (= (length expected-result) (length actual-result)))
  (flet ((is-expected-entry (expected-entry actual-entry)
           (destructuring-bind (expected-line . expected-wad) expected-entry
             (destructuring-bind (actual-line . actual-wad) actual-entry
               (is (= expected-line actual-line))
               (let ((expected-wad (append expected-wad '(:ignore-children))))
                 (is-result expected-wad actual-wad :input input))))))
    (mapc #'is-expected-entry expected-result actual-result)))

(defun query-test-cases (query-function)
  (labels ((relation-variant (cache line column start-relation end-relation
                              expected-result)
             ;; Perform a query for the given
             ;; * input (implicit in CACHE)
             ;; * cache division into prefix and suffix (implicit in CACHE)
             ;; * query position and
             ;; * start/end relation variant
             ;; and check the result. Outermost wad is first in
             ;; RESULT.
             (let ((result (funcall query-function cache line column
                                    start-relation end-relation)))
               (is-query-result expected-result result)))
           (query-variant (cache line column relation-variants)
             ;; Go through the provided start/end relation variants
             ;; with their respective expected results.
             (loop for (relations expected-results) in relation-variants
                   do (loop for (start-relation end-relation) in relations
                            do (relation-variant cache line column
                                                 start-relation end-relation
                                                 expected-results))))
           (cache-configuration (cache query-variants)
             ;; Go through query locations with their respective
             ;; expected results.
             (loop for (line column . relation-variants) in query-variants
                   do (query-variant cache line column relation-variants)))
           (test-case (input &rest query-variants)
             (multiple-value-bind (analyzer cache) (prepared-analyzer input)
               (update-cache analyzer cache)
               ;; Go through all possible divisions of CACHE into
               ;; prefix and suffix.
               (loop for i to (length (inc::prefix cache))
                     unless (zerop i)
                     do (inc::prefix-to-suffix cache)
                     do (cache-configuration cache query-variants)))))
    (test-case ""
               '(0 0
                 (((< <) (< <=) (<= <) (<= <=))
                  ())))
    (test-case "((1 2 3))"
               '(0 4
                 (((< <))
                  ((0 . (inc:cons-wad ((0 1) (0 8)) ()))
                   (0 . (inc:cons-wad ((0 0) (0 9)) ()))))))
    (test-case "((1)) ((2))   ((3))"
               '(0 5
                 (((< <) (<= <))
                  ())
                 (((< <=) (<= <=))
                  ((0 . (inc:cons-wad ((0 0) (0 5)) ())))))
               '(0 8
                 (((< <) (< <=))
                  ((0 . (inc:cons-wad ((0 7) (0 10)) ()))
                   (0 . (inc:cons-wad ((0 6) (0 11)) ()))))
                 (((<= <) (<= <=))
                  ((0 . (inc:atom-wad ((0 8) (0  9)) ()))
                   (0 . (inc:cons-wad ((0 7) (0 10)) ()))
                   (0 . (inc:cons-wad ((0 6) (0 11)) ())))))
               '(0 12
                 (((< <) (< <=) (<= <) (<= <=))
                  ())))))

(test map-wads-containing-position.smoke
  "Smoke test for the `map-wads-containing-position' and
`find-wad-containing-position' functions."
  (query-test-cases
   (lambda (cache line column start-relation end-relation)
     (let ((result '()))
       (inc:map-wads-containing-position
        (lambda (absolute-line-number wad)
          (push (cons absolute-line-number wad) result))
        cache line column :start-relation start-relation
                          :end-relation   end-relation)
       (nreverse result)))))

(test find-wads-containing-position.smoke
  "Smoke test for the `find-wads-containing-position' function."
  (query-test-cases
   (lambda (cache line column start-relation end-relation)
     (inc:find-wads-containing-position
      cache line column :start-relation start-relation
                        :end-relation   end-relation))))

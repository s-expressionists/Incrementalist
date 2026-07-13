(cl:in-package #:incrementalist.test)

(def-suite* :incrementalist.cache
  :in :incrementalist)

;;;; Utilities for query function tests

;;; EXPECTED-RESULT is a list of elements of the form
;;;
;;;   (EXPECTED-LINE . EXPECTED-WAD)
;;;
;;; where EXPECTED-WAD is a specification of the form consumed by
;;; `is-result', that is
;;;
;;;   (CLASS ((START-LINE START-COLUMN) (END-LINE END-COLUMN)) PLIST)
(defun is-query-result (expected-result actual-result &key input arguments)
  (is-result-count= expected-result actual-result
                    :input input :arguments arguments)
  (flet ((is-expected-entry (expected-entry actual-entry)
           (destructuring-bind (expected-line . expected-element) expected-entry
             (destructuring-bind (actual-line . actual-element) actual-entry
               (is (= expected-line actual-line)
                   "~@<For input~@:_~@:_~
                    ~S~
                    ~@:_~@:_~
                    and arguments ~S, expected line ~D but got ~D~@:>"
                   input arguments expected-line actual-line)
               (let ((expected-wad (append expected-element
                                           '(:ignore-children))))
                 (is-result expected-wad actual-element :input input))))))
    (mapc #'is-expected-entry expected-result actual-result)))

(defun map-query-test-cases (function test-cases)
  (labels ((cache-configurations (input cache arguments expected)
             (map-prefix-suffix-splits
              (lambda (cache split-number)
                (funcall function input cache split-number arguments expected))
              cache))
           (query-case (input cache query-case)
             (destructuring-bind (argument-specifications expected) query-case
               (labels ((rec (args remaining)
                          (if (null remaining)
                              (cache-configurations
                               input cache (reverse args) expected)
                              (destructuring-bind (first &rest rest)
                                  remaining
                                (if (consp first)
                                    (mapc (lambda (arg)
                                            (rec (list* arg args) rest))
                                          first)
                                    (rec (list* first args) rest))))))
                 (rec '() argument-specifications))))
           (input-case (test-case)
             (destructuring-bind (input . query-cases) test-case
               (let ((expanded-input (format nil input)))
                 (multiple-value-bind (analyzer cache)
                     (prepared-analyzer expanded-input)
                   (update-cache analyzer cache)
                   (mapc (a:curry #'query-case input cache) query-cases))))))
    (mapc #'input-case test-cases)))

;;;; Query function tests

(defun location-query-test-cases (query-function)
  ;; A collection of inputs, query locations and relations with
  ;; expected results.
  (map-query-test-cases
   (lambda (input cache split-number arguments expected)
     (declare (ignore split-number))
     (let ((result (apply query-function cache arguments)))
       (is-query-result expected result :input input :arguments arguments)))
   '(;; Empty
     (""
      ((0 0 :start-relation (< <=) :end-relation (< <=))
       ()))
     ;; Single line
     ("((1 2 3))"
      ((0 4 :start-relation < :end-relation (< <=))
       ((0 . (inc:cons-wad ((0 1) (0 8)) ()))
        (0 . (inc:cons-wad ((0 0) (0 9)) ()))))
      ((0 4 :start-relation <= :end-relation (< <=))
       ((0 . (inc:atom-wad ((0 4) (0 5)) ()))
        (0 . (inc:cons-wad ((0 1) (0 8)) ()))
        (0 . (inc:cons-wad ((0 0) (0 9)) ())))))
     ("((1)) ((2))   ((3))"
      ((0 5 :start-relation (< <=) :end-relation <)
       ())
      ((0 5 :start-relation (< <=) :end-relation <=)
       ((0 . (inc:cons-wad ((0 0) (0 5)) ()))))
      ((0 8 :start-relation < :end-relation (< <=))
       ((0 . (inc:cons-wad ((0 7) (0 10)) ()))
        (0 . (inc:cons-wad ((0 6) (0 11)) ()))))
      ((0 8 :start-relation <= :end-relation (< <=))
       ((0 . (inc:atom-wad ((0 8) (0  9)) ()))
        (0 . (inc:cons-wad ((0 7) (0 10)) ()))
        (0 . (inc:cons-wad ((0 6) (0 11)) ()))))
      ((0 12 :start-relation (< <=) :end-relation (< <=))
       ())))))

(test map-wads-containing-position.smoke
  "Smoke test for the `map-wads-containing-position' function."
  (location-query-test-cases
   (lambda (&rest args)
     (let ((result '()))
       (apply #'inc:map-wads-containing-position
              (lambda (absolute-line-number wad)
                (push (cons absolute-line-number wad) result))
              args)
       (nreverse result)))))

(test find-wads-containing-position.smoke
  "Smoke test for the `find-wads-containing-position' function."
  (location-query-test-cases #'inc:find-wads-containing-position))

(test maps-wads-and-spaces.smoke
  "Smoke test for the `map-wads-and-spaces' function."
  (map-query-test-cases
   (lambda (input cache split-number arguments expected)
     (declare (ignore split-number))
     (destructuring-bind (start-line end-line) arguments
       ;; We accumulate in RESULT in reverse order but
       ;; `map-wads-and-spaces' also reports in reverse order so
       ;; actual order in RESULT corresponds to the order in the input
       ;; string.
       (let ((result '()))
         (is (equal
              (values nil)
              (inc:map-wads-and-spaces
               cache start-line end-line
               (lambda (wad)
                 (push (cons (inc:absolute-start-line wad) wad) result))
               (lambda (line-number start-column end-column)
                 (declare (ignore line-number start-column end-column))
                 ;; We will check reported spaces later.
                 #++ (push (cons line-number (cons start-column end-column))
                           result)))))
         (is-query-result expected result :input input :arguments arguments))))
   '(;; Empty
     (""
      ((0 0) ()))
     ;; Smoke test
     ("   (1 2 3)    ~@
       ;foo~@
       ;bar~@
       ~2@T:foo"
      ((0 0)
       ((0 . (inc:cons-wad ((0 3) (0 10)) ()))))
      ((1 1)
       ((1 . (inc:semicolon-comment-wad ((1 0) (2 0)) ()))))
      ((2 2)
       ((1 . (inc:semicolon-comment-wad ((1 0) (2 0)) ()))
        (2 . (inc:semicolon-comment-wad ((2 0) (3 0)) ()))))
      ((3 3)
       ((2 . (inc:semicolon-comment-wad ((2 0) (3 0)) ()))
        (3 . (inc:atom-wad ((3 2) (3 6)) ()))))
      ((1 3)
       ((1 . (inc:semicolon-comment-wad ((1 0) (2 0)) ()))
        (2 . (inc:semicolon-comment-wad ((2 0) (3 0)) ()))
        (3 . (inc:atom-wad ((3 2) (3 6)) ())))))
     ;; Multi-line `cons-wad'
     ("(1~@
       2~@
       3)~@
       :foo"
      ((0 0)
       ((0 . (inc:cons-wad ((0 0) (2 2)) ()))))
      ((1 1)
       ((0 . (inc:cons-wad ((0 0) (2 2)) ()))))
      ((2 2)
       ((0 . (inc:cons-wad ((0 0) (2 2)) ()))))
      ((3 3)
       ((3 . (inc:atom-wad ((3 0) (3 4)) ()))))
      ((1 3)
       ((0 . (inc:cons-wad ((0 0) (2 2)) ()))
        (3 . (inc:atom-wad ((3 0) (3 4)) ())))))
     ;; Empty lines between cache elements.
     ("foo~@
       ~@
       ~@
       bar"
      ((0 2)
       ((0 . (inc:atom-wad ((0 0) (0 3)) ()))))
      ((1 2)
       ())
      ((2 3)
       ((3 . (inc:atom-wad ((3 0) (3 3)) ()))))
      ((3 3)
       ((3 . (inc:atom-wad ((3 0) (3 3)) ()))))
      ((0 3)
       ((0 . (inc:atom-wad ((0 0) (0 3)) ()))
        (3 . (inc:atom-wad ((3 0) (3 3)) ()))))))))

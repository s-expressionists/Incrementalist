(cl:in-package #:incrementalist.test)

;;; Constructing buffer, analyzer and cache

(defpackage #:incrementalist.test.test-package (:use))
(defpackage #:incrementalist.test.using-package (:use #:cl))

;;; TODO (c:items buffer)
(defun buffer-string (buffer)
  (with-output-to-string (stream)
    (loop :for line-number :below (b:line-count buffer)
          :for line        =      (b:find-line buffer line-number)
          :for content     =      (b:items line)
          :unless (zerop line-number)
            :do (terpri stream)
          :do (map nil (a:rcurry #'write-char stream) content))))

(defun prepared-buffer (content)
  (multiple-value-bind (buffer cursor) (b:make-buffer)
    (b:insert-items-at cursor content :line-separator #\Newline)
    (values buffer cursor)))

(defun prepared-analyzer
    (buffer-content &key (initial-package      "INCREMENTALIST.TEST.TEST-PACKAGE")
                         (initial-reader-state (inc:make-initial-reader-state
                                                :package initial-package)))
  (multiple-value-bind (buffer cursor) (prepared-buffer buffer-content)
    (let* ((analyzer (make-instance 'inc:analyzer
                                    :buffer               buffer
                                    :initial-reader-state initial-reader-state))
           (cache    (inc:cache analyzer)))
      (values analyzer cache buffer cursor))))

(defun update-cache (analyzer cache)
  (inc:update analyzer)
  (append (reverse (inc::prefix cache)) (inc::suffix cache)))

(defun parse-result (buffer-content &rest args
                                    &key initial-package initial-reader-state)
  (declare (ignore initial-package initial-reader-state))
  (multiple-value-bind (analyzer cache)
      (apply #'prepared-analyzer buffer-content args)
    (update-cache analyzer cache)))

(defun map-prefix-suffix-splits (function cache)
  ;; Go through all possible divisions of CACHE into prefix and
  ;; suffix.
  (loop :for i :from 0 :to (+ (length (inc::prefix cache))
                              (length (inc::suffix cache)))
        :do ;; Set up the cache to contain I WADs in the prefix.  Do a
            ;; full adjustment with potentially multiple iterations
            ;; each time since FUNCTION is allowed to mutate CACHE.
            (loop :for count = (length (inc::prefix cache))
                  :do (cond ((< count i)
                             (inc::suffix-to-prefix cache))
                            ((> count i)
                             (inc::prefix-to-suffix cache))
                            (t
                             (loop-finish))))
            ;; Call FUNCTION with the prepared cache split.
            (funcall function cache i)))

;;; Reporting utilities

(defun format-node (stream root-and-node &optional colon? at?)
  (declare (ignore colon? at?))
  (destructuring-bind (root . node) root-and-node
    (utilities.print-tree:print-tree
     stream root
     (utilities.print-tree:make-node-printer
      (lambda (stream depth node*)
        (declare (ignore depth))
        (princ node* stream)
        (when (eq node* node)
          (write-string " ***" stream))
        nil)
      nil
      (lambda (wad)
        (unless (typep wad 'inc:error-wad)
          (append (inc:errors wad) (inc:children wad))))))))

(defun format-input-and-arguments
    (stream test-case-info &optional colon? at?)
  (declare (ignore colon? at?))
  (destructuring-bind (&key input arguments &allow-other-keys) test-case-info
    (format stream "~@[input~@:_~@:_~
                    ~S~
                    ~@:_~@:_~]~@[and arguments ~:S~]"
            input arguments)))

(defun format-test-case-info (stream test-case-info &optional colon? at?)
  (declare (ignore colon? at?))
  (destructuring-bind (&key comment node root &allow-other-keys) test-case-info
    (format stream "~/incrementalist.test::format-input-and-arguments/~
                    ~@[ and ~A~] and result~@:_~@:_~
                    ~/incrementalist.test::format-node/~
                    ~@:_~@:_"
            test-case-info comment (cons root node))))

(defvar *test-case-info* nil)

(defun format-results (stream results &optional colon? at?)
  (declare (ignore colon? at?))
  (loop :for result :in results
        :do (fresh-line stream)
            (format-node stream (cons result nil))))

;;; The following functions (and helpers) check results (trees or
;;; forests of wads) in a style that matches fiveam assertions with
;;; `fiveam:is'.  The functions form two groups which perform similar
;;; checks but on different kinds of arguments:
;;;
;;;  Arguments               | "spec" vs result | result vs result |
;;;  ------------------------+------------------+------------------+
;;;  Wad location and type   | `is-wad-data'    | `is-wad-data='   |
;;;  Error location and type | `is-error'       | `is-error='      |
;;;  Raw value               | `is-raw'         | `is-raw='        |
;;;  Single result           | `is-result'      | `is-result='     |
;;;  List of results         | `are-results'    | `are-results='   |

(defun wad-location (wad)
  (let* ((start-line (inc:absolute-start-line wad))
         (end-line   (+ start-line (inc:height wad))))
    (list (list start-line (inc:start-column wad))
          (list end-line   (inc:end-column wad)))))

(defmacro is-with-node ((predicate expected actual) label)
  `(is (,predicate ,expected ,actual)
       "~@<For ~/incrementalist.test::format-test-case-info/expected ~A to ~
        be ~S but ~A is ~S.~@:>"
       *test-case-info* ,label ,expected ,label ,actual))

(defun is-sequence (child-predicate expected-sequence actual-sequence label)
  (let ((label-control (concatenate 'string "~1:*" label "~*")))
    (is (= (length expected-sequence) (length actual-sequence))
        "~@<For ~/incrementalist.test::format-test-case-info/expected the wad ~
         to have ~D ~@? but it has ~D ~@?.~@:>"
        *test-case-info*
        (length expected-sequence) label-control
        (length actual-sequence)   label-control))
  (mapc child-predicate expected-sequence actual-sequence))

(defun is-wad-data (expected-type expected-location wad)
  (is (typep wad expected-type)
      "~@<For ~/incrementalist.test::format-test-case-info/expected the wad ~
       to be of type ~A but it is of type ~A.~@:>"
      *test-case-info* expected-type (class-name (class-of wad)))
  (is-with-node (equal expected-location (wad-location wad))
                "location of the wad"))

(defun is-wad-data= (expected-wad actual-wad)
  (is-with-node (a:type= (type-of expected-wad) (type-of actual-wad)) "type of")
  (is-with-node (equal (wad-location expected-wad) (wad-location actual-wad))
                "location of the wad"))

(defun is-error (expected-error wad)
  (let ((*test-case-info* (list* :node wad *test-case-info*)))
    (destructuring-bind (expected-location expected-condition-type) expected-error
      (is-wad-data 'inc:error-wad expected-location wad)
      (let ((condition (inc:condition wad)))
        (is (typep condition expected-condition-type)
            "~@<For ~/incrementalist.test::format-test-case-info/~
             expected the condition in the error wad to be of type ~
             ~A but it is of type ~A.~@:>"
            *test-case-info*
            expected-condition-type
            (class-name (class-of condition)))))))

(defun is-error= (expected-error-wad actual-error-wad)
  (let ((*test-case-info* (list* :node actual-error-wad *test-case-info*)))
    (is-wad-data= expected-error-wad actual-error-wad)
    (let ((expected-condition (inc:condition expected-error-wad))
          (actual-condition   (inc:condition actual-error-wad)))
      (is-with-node (a:type= (class-of expected-condition)
                             (class-of actual-condition))
                    "condition type"))))

(defun is-raw (expected-raw raw)
  (if (consp expected-raw)
      (destructuring-bind (expected-type &key ((:symbol expected-symbol)
                                               nil
                                               expected-symbol-supplied?))
          expected-raw
        (is (typep raw expected-type))
        (when expected-symbol-supplied?
          (destructuring-bind (expected-package-name expected-name)
              expected-symbol
            (let ((actual-package-name (inc:package-name raw))
                  (actual-name         (inc:name raw)))
              (is-with-node (string= expected-package-name actual-package-name)
                            "package name of the symbol token of the node")
              (is-with-node (string= expected-name actual-name)
                            "name of the symbol token of the node")))))
      (is-with-node (equal expected-raw raw) "raw value of the node")))

(defun is-raw= (expected actual)
  (labels ((rec (expected actual)
             (is-with-node (a:type= (type-of expected) (type-of actual))
                           "type of the raw value of the node")
             (when (a:type= (type-of expected) (type-of actual))
               (typecase expected
                 (cons
                  (rec (car expected) (car actual))
                  (rec (cdr expected) (cdr actual)))
                 (inc:symbol-token
                  (is-with-node (string= (inc:package-name expected)
                                         (inc:package-name actual))
                                "package name of the symbol token of the node")
                  (is-with-node (string= (inc:name expected) (inc:name actual))
                                "name of the symbol token of the node"))
                 (t
                  (is-with-node (equal expected actual)
                                "raw value of the node"))))))
    (rec expected actual)))

(defun is-result (expected root-result)
  (labels
      ((rec (expected result)
         (is-true (dep:validp result))
         (is (null (dep:invalid result)))
         (destructuring-bind
             (expected-type expected-location
              &optional ((&key ((:errors expected-errors) '())
                               ((:raw expected-raw)
                                nil
                                expected-raw-supplied?)
                               ;; feature-expression is for `conditional-wad's
                               ((:feature-expression expected-feature-expression)
                                nil
                                expected-feature-expression-supplied?)
                               ((:misspelled expected-misspelled)
                                nil
                                expected-misspelled-supplied?))
                         '())
              &rest     expected-children)
             expected
           (let ((*test-case-info* (list* :node result *test-case-info*)))
             ;; Assertions for type and location
             (is-wad-data expected-type expected-location result)
             ;; Assertions for errors
             (is-sequence #'is-error expected-errors (inc:errors result)
                          "error~:P")
             ;; Assertions for raw value
             (when (and expected-raw-supplied?
                        (typep result expected-type))
               (let ((raw (cst:raw result)))
                 (if (equal expected-raw '(inc:existing-symbol-token
                                           :symbol ("COMMON-LISP" "NIL")))
                     (is-true (cst:null result))
                     (is-false (cst:null result)))
                 (is-raw expected-raw raw)))
             ;; Assertions for feature expression of `conditional-wad's
             (when expected-feature-expression-supplied?
               (let ((feature-expression (inc:feature-expression result)))
                 (is-raw expected-feature-expression feature-expression)))
             ;; Assertions for `labeled-object-reference-wad'
             (when (typep result 'inc:labeled-object-reference-wad)
               (let* ((target     (eclector.concrete-syntax-tree:target result))
                      (definition (inc:parent target)))
                 (is-true (typep definition 'inc:labeled-object-definition-wad))
                 (is (eq (cst:raw definition) (cst:raw result)))))
             ;; Spelling.
             (when expected-misspelled-supplied?
               (is-with-node (eq expected-misspelled (inc:misspelled result))
                             "misspelled status"))
             ;; Recursively check children
             (unless (equal expected-children '(:ignore-children))
               (is-sequence #'rec expected-children (inc:children result)
                            "~:*child~[ren~;~:;ren~]"))))))
    (let ((*test-case-info* (list* :root root-result *test-case-info*)))
      (if (typep root-result #1='inc:wad)
          (rec expected root-result)
          (fiveam:fail "~@<For ~/incrementalist.test::format-input-and-arguments/~
                        , expected ~S to be of type ~S but it is of type ~S.~@:>"
                       *test-case-info* root-result #1# (type-of root-result))))))

(defun is-result= (expected-result actual-result)
  (is (a:type= (type-of expected-result) (type-of actual-result)))
  (labels ((rec (expected result)
             (let ((*test-case-info* (list* :node result
                                            :root actual-result
                                            *test-case-info*)))
               (is-true (dep:validp result))
               (is (null (dep:invalid result)))
               ;; Compare type and location
               (is-wad-data= expected result)
               ;; Compare raw values
               (when (typep expected 'cst:cst)
                 (is-raw= (cst:raw expected) (cst:raw result)))
               ;; Compare errors
               (is-sequence #'is-error= (inc:errors expected) (inc:errors result)
                            "error~:P")
               ;; Compare children
               (is-sequence #'rec (inc:children expected) (inc:children result)
                            "~:*child~[ren~;~:;ren~]"))))
    (rec expected-result actual-result)))

(defun is-result-count= (expected-results actual-results)
  (is (= (length expected-results) (length actual-results))
      "~@<For ~/incrementalist.test::format-input-and-arguments/~
       , expected ~D result~:P but there ~[are~;is~:;are~] ~:*~D result~:P.~@:>"
      *test-case-info* (length expected-results) (length actual-results)))

(defun are-results (expected-results actual-results)
  (is-result-count= expected-results actual-results)
  (mapc #'is-result expected-results actual-results))

(defun are-results= (expected-results actual-results)
  (is-result-count= expected-results actual-results)
  (mapc #'is-result= expected-results actual-results))

;;; Ensure that the cells in the initial reader state of ANALYZER do
;;; not have any users that are not contained in the wad forest
;;; RESULTS.
(defun no-dangling-nodes (results analyzer)
  (let ((nodes (make-hash-table :test #'eq)))
    (labels ((rec (node)
               (setf (gethash node nodes) t)
               (inc:map-children #'rec node)))
      (mapc #'rec results))
    (loop :for cell :in (inc::initial-reader-state analyzer)
          :do (dep:map-users (lambda (user)
                               (when (not (nth-value 1 (gethash user nodes)))
                                 (fiveam:fail "dangling node ~A in cell ~A"
                                              user cell)))
                             cell))))

;;; Utilities for analysis test cases

(defun expected-semicolon-comment-wad
    (location
     &key (words (destructuring-bind ((start-line start-column)
                                      (end-line   end-column))
                     location
                   `((inc:word-wad ((,start-line ,(+ start-column 2))
                                    (,end-line   ,end-column)))))))
  `(inc:semicolon-comment-wad ,location () ,@words))

(defun expected-consing-dot-wad (location)
  `(inc:atom-wad ,location (:raw (symbol)))) ; TODO make this a token?

(defun expected-symbol-wad
    (location symbol-name
     &key (package-name "INCREMENTALIST.TEST.TEST-PACKAGE")
          (token-class  'inc:non-existing-symbol-token)
          (words        (cond ((equal package-name "COMMON-LISP")
                               '())
                              ((> (length symbol-name) 1)
                               `((inc:word-wad ,location)))
                              (t
                               `((inc:text-wad ,location))))))
  `(inc:atom-wad ,location
    (:raw (,token-class :symbol (,package-name ,symbol-name)))
    ,@words))

(defun analysis-test-case (input-and-arguments expected-result)
  (destructuring-bind (input &rest arguments)
      (a:ensure-list input-and-arguments)
    (let ((actual-result (apply #'parse-result input arguments)))
      (let ((*test-case-info* (list :input input :arguments arguments)))
        (are-results expected-result actual-result)))))

(defmacro analysis-cases (() &body cases)
  `(progn
     ,@(mapcar (lambda (case)
                 (destructuring-bind (input expected-result) case
                   `(analysis-test-case ,input ,expected-result)))
               cases)))

;;; Utilities for editing test cases

;;; Apply EDIT using CURSOR. EDIT is of the form
;;;
;;;   EDIT ::=   (progn EDIT*)
;;;            | (:move (line-number column-number))
;;;            | (:delete number-of-items)
;;;            | string
;;;            | (:insert (line-number column-number) string)
;;;            | (:erase (line-number column-number) number-of-items)
;;;            | (:poke (line-number column-number))
;;;            | (:replace (line-number column-number) string)
;;;
;;; AFTER-STEP, if supplied, is a function of no arguments that is called after
;;; each buffer modification (not after each edit).
(defun apply-edit (cursor edit &key after-step)
  (labels ((maybe-notify ()
             (when after-step
               (funcall after-step)))
           (apply-one (edit)
             (etypecase edit
               ((cons (eql progn))
                (mapcar #'apply-one (rest edit)))
               ((cons (eql :move))
                (destructuring-bind ((line-number column-number)) (rest edit)
                  (let* ((buffer (b:buffer cursor))
                         (line   (b:find-line buffer line-number)))
                    (b:detach cursor)
                    (b:attach cursor line column-number)
                    (maybe-notify))))
               ((cons (eql :delete))
                (destructuring-bind (amount) (rest edit)
                  (loop :repeat amount
                        :do (if (b:end-of-line-p cursor)
                                (b:join-line cursor)
                                (b:delete-item-after cursor))
                            (maybe-notify))))
               (string
                (loop :for c :across edit
                      :do (case c
                            (#\Newline (b:split-line cursor))
                            (t         (b:insert-item-at cursor c)))
                          (maybe-notify)))
               ((cons (eql :insert))
                (destructuring-bind (location string) (rest edit)
                  (apply-one `(progn (:move ,location) ,string))))
               ((cons (eql :erase))
                (destructuring-bind (location number-of-items) (rest edit)
                  (apply-one `(progn
                                (:move ,location)
                                (:delete ,number-of-items)))))
               ((cons (eql :poke))
                (destructuring-bind (location) (rest edit)
                  (apply-one `(progn
                                (:insert ,location " ")
                                (:erase ,location 1)))))
               ((cons (eql :replace))
                (destructuring-bind (location string) (rest edit)
                  (apply-one `(progn
                                (:move ,location)
                                (:delete ,(length string))
                                ,string)))))))
    (apply-one edit)))

(defun edits-test-case (&rest edits-and-results)
  (flet ((do-it (step-mode map-wads-and-spaces?)
           ;; Sequentially apply all edits updating the cache and checking the
           ;; result after each edit.
           (loop :with (analyzer cache buffer cursor) = (multiple-value-list
                                                         (prepared-analyzer ""))
                 :for i :from 0
                 :for (edit expected-results) :on edits-and-results :by #'cddr
                 :do (apply-edit cursor edit
                                 :after-step (lambda ()
                                               (when (eq step-mode :small-step)
                                                 (update-cache analyzer cache))))
                     (when (eq step-mode :big-step)
                       (update-cache analyzer cache))
                     (let ((results (update-cache analyzer cache)))
                       (when map-wads-and-spaces?
                         (inc:map-wads-and-spaces
                          #1=(lambda (&rest args) (declare (ignore args))) #1#
                          cache))
                       (unless (eq expected-results :ignore)
                         (let ((input   (buffer-string buffer))
                               (comment (format nil "~A mode, after edit ~D"
                                                step-mode (1+ i))))
                           (let ((*test-case-info* (list :input   input
                                                         :comment comment)))
                             (are-results expected-results results))))))))
    (do-it :small-step nil) ; update cache after each item change
    (do-it :big-step   nil) ; update cache after each edit
    (do-it :small-step t)   ; like above but call `map-wads-and-spaces' after
    (do-it :big-step   t))) ; each update

(defmacro edits-cases (() &body cases)
  `(progn ,@(mapcar (lambda (case) `(edits-test-case ,@case)) cases)))

(cl:in-package #:incrementalist.test)

;;; Constructing buffer, analyzer and cache

(defpackage incrementalist.test.test-package (:use))

(defun insert-string (cursor string)
  (loop :for c :across string
        :do (case c
              (#\Newline (cluffer:split-line cursor))
              (t         (cluffer:insert-item cursor c)))))

(defun buffer-string (buffer)
  (with-output-to-string (stream)
    (loop :for line-number :below (cluffer:line-count buffer)
          :for line        =      (cluffer:find-line buffer line-number)
          :for content     =      (cluffer:items line)
          :unless (zerop line-number)
            :do (terpri stream)
          :do (map nil (a:rcurry #'write-char stream) content))))

(defun prepared-buffer (content)
  (let* ((line   (make-instance 'cluffer-standard-line:open-line))
         (buffer (make-instance 'cluffer-standard-buffer:buffer
                                :initial-line line))
         (cursor (make-instance 'cluffer-standard-line:right-sticky-cursor)))
    (cluffer:attach-cursor cursor line 0)
    (insert-string cursor content)
    (values buffer cursor)))

(defun prepared-analyzer (&optional (buffer-content ""))
  (multiple-value-bind (buffer cursor) (prepared-buffer buffer-content)
    (let* ((analyzer (make-instance 'inc:analyzer :buffer buffer))
           (cache    (inc:cache analyzer)))
      (values analyzer cache buffer cursor))))

(defun update-cache (analyzer cache)
  (let ((*package* (find-package '#:incrementalist.test.test-package)))
    (inc:update analyzer))
  (append (reverse (inc::prefix cache)) (inc::suffix cache)))

(defun parse-result (buffer-content)
  (multiple-value-bind (analyzer cache) (prepared-analyzer buffer-content)
    (update-cache analyzer cache)))

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

;;; Predicates

(defun is-sequence (child-predicate expected-sequence actual-sequence result-info label
                    &key input)
  (let ((label-control (concatenate 'string "~1:*" label "~*")))
    (is (= (length expected-sequence) (length actual-sequence))
        "~@<For~@[ input~@:_~@:_~
         ~S~
         ~@:_~@:_ and~] result~@:_~@:_~
         ~/incrementalist.test::format-node/~
         ~@:_~@:_expected the wad to have ~D ~@? but it has ~D ~@?.~@:>"
        input result-info
        (length expected-sequence) label-control
        (length actual-sequence)   label-control))
  (mapc child-predicate expected-sequence actual-sequence))

(defun is-wad-data (expected-type expected-location wad result-info
                    &key input)
  (is (typep wad expected-type)
      "~@<For~@[ input~@:_~@:_~
       ~S~
       ~@:_~@:_ and~] result~@:_~@:_~
       ~/incrementalist.test::format-node/~
       ~@:_~@:_expected the wad to be of type ~A but it is of type ~A.~@:>"
      input result-info expected-type (class-name (class-of wad)))
  (let* ((start-line      (inc:absolute-start-line wad))
         (end-line        (+ start-line (inc:height wad)))
         (actual-location (list (list start-line (inc:start-column wad))
                                (list end-line   (inc:end-column wad)))))
    (is (equal expected-location actual-location)
        "~@<For~@[ input~@:_~@:_~
         ~S~
         ~@:_~@:_ and~] result~@:_~@:_~
         ~/incrementalist.test::format-node/~
         ~@:_~@:_expected location of the wad to be ~S but its location is ~
         ~S.~@:>"
        input result-info expected-location actual-location)))

(defun is-error (expected-error wad result-info &key input)
  (let ((result-info (cons (car result-info) wad)))
    (destructuring-bind (expected-location expected-condition-type) expected-error
      (is-wad-data 'inc:error-wad expected-location wad result-info
                   :input input)
      (let ((condition (inc:condition wad)))
        (is (typep condition expected-condition-type)
            "~@<For~@[ input~@:_~@:_~
             ~S~
             ~@:_~@:_ and~] result~@:_~@:_~
             ~/incrementalist.test::format-node/~
             ~@:_~@:_expected the condition in the error wad to be of type ~
             ~A but it is of type ~A.~@:>"
            input result-info
            expected-condition-type
            (class-name (class-of condition)))))))

(defun is-raw (expected-raw raw result-info &key input)
  (if (consp expected-raw)
      (destructuring-bind (expected-type &key ((:symbol expected-symbol)
                                               nil
                                               expected-symbol-supplied?))
          expected-raw
        (is (typep raw expected-type))
        (when expected-symbol-supplied?
          (destructuring-bind
              (expected-package-name expected-name)
              expected-symbol
            (let ((actual-package-name (inc:package-name raw))
                  (actual-name         (inc:name raw)))
              (is (string= expected-package-name
                           actual-package-name)
                  "~@<For~@[ input~@:_~@:_~
                   ~S~
                   ~@:_~@:_, and~] result~@:_~@:_~
                   ~/incrementalist.test::format-node/~
                   ~@:_~@:_expected symbol token of the node to have ~
                   package name ~S but the package name is ~S.~@:>"
                  input result-info expected-package-name actual-package-name)
              (is (string= expected-name actual-name)
                  "~@<For~@[ input~@:_~@:_~
                   ~S~
                   ~@:_~@:_, and~] result~@:_~@:_~
                   ~/incrementalist.test::format-node/~
                   ~@:_~@:_expected symbol token of the node to have ~
                   package name ~S but the package name is ~S.~@:>"
                  input result-info expected-name actual-name)))))
      (is (equal expected-raw raw)
          "~@<For~@[ input~@:_~@:_~
           ~S~
           ~@:_~@:_, and~] result~@:_~@:_~
           ~/incrementalist.test::format-node/~
           ~@:_~@:_expected raw value of the node to be ~S but the raw value ~
           is ~S.~@:>"
          input result-info expected-raw raw)))

(defun is-result (expected root-result &key input)
  (labels
      ((rec (expected result)
         (destructuring-bind
             (expected-type expected-location
              &optional ((&key ((:errors expected-errors) '())
                               ((:raw expected-raw)
                                nil
                                expected-raw-supplied?)
                               ;; expression is for sharpsign-{plus,minus}-wad
                               ((:expression expected-expression)
                                nil
                                expected-expression-supplied?))
                         '())
              &rest     expected-children)
             expected
           (let ((result-info (cons root-result result)))
             ;; Assertions for type and location
             (is-wad-data expected-type expected-location result result-info
                          :input input)
             ;; Assertions for errors
             (is-sequence (lambda (expected-error actual-error)
                            (is-error expected-error actual-error result-info))
                          expected-errors (inc:errors result)
                          result-info "error~:P" :input input)
             ;; Assertions for raw value
             (when (and expected-raw-supplied?
                        (typep result expected-type))
               (let ((raw (cst:raw result)))
                 (if (equal expected-raw '(inc:existing-symbol-token
                                           :symbol ("COMMON-LISP" "NIL")))
                     (is-true (cst:null result))
                     (is-false (cst:null result)))
                 (is-raw expected-raw raw result-info :input input)))
             ;; Assertions for expression of `sharpsign-plus-wad' and
             ;; `sharpsign-minus-wad'.
             (when expected-expression-supplied?
               (let ((expression (inc:expression result)))
                 (is-raw expected-expression expression result-info
                         :input input)))
             ;; Recursively check children
             (unless (equal expected-children '(:ignore-children))
               (is-sequence #'rec expected-children (inc:children result)
                            result-info "~:*child~[ren~;~:;ren~]"
                            :input input))))))
    (rec expected root-result)))

(defun are-results (expected-results actual-results &key input)
  (is (= (length expected-results) (length actual-results))
      "~@<~@[For input~@:_~@:_~
       ~S~
       ~@:_~@:_,~] ~
       expected ~D result~:P but there ~[are~;is~:;are~] ~:*~D result~:P.~@:>"
      input (length expected-results) (length actual-results))
  (mapc (lambda (expected-result actual-result)
          (is-result expected-result actual-result :input input))
        expected-results actual-results))

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

(defun analysis-test-case (input expected-result)
  (are-results expected-result (parse-result input) :input input))

(defmacro analysis-cases (() &body cases)
  `(progn
     ,@(mapcar (lambda (case)
                 (destructuring-bind (input expected-result) case
                   `(analysis-test-case ,input ,expected-result)))
               cases)))

;;; Utilities for editing test cases

;;; Apply EDIT using CURSOR. EDIT is of the form
;;;
;;;   EDIT ::= | (progn EDIT*)
;;;            | (:move line-number column-number)
;;;            | (:delete number-of-items)
;;;            | string
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
                (destructuring-bind (line-number column-number) (rest edit)
                  (let* ((buffer (cluffer:buffer cursor))
                         (line   (cluffer:find-line buffer line-number)))
                    (cluffer:detach-cursor cursor)
                    (cluffer:attach-cursor cursor line column-number)
                    (maybe-notify))))
               ((cons (eql :delete))
                (let ((amount (second edit)))
                  (loop :repeat amount :do (cluffer:delete-item cursor)
                                           (maybe-notify))))
               (string
                (loop :for c :across edit
                      :do (case c
                            (#\Newline (cluffer:split-line cursor))
                            (t         (cluffer:insert-item cursor c)))
                          (maybe-notify))))))
    (apply-one edit)))

(defun edits-test-case (&rest edits-and-results)
  (flet ((do-it (mode)
           ;; Sequentially apply all edits updating the cache and checking the
           ;; result after each edit.
           (loop :with (analyzer cache buffer cursor) = (multiple-value-list
                                                         (prepared-analyzer ""))
                 :for i :from 0
                 :for (edit expected-results) :on edits-and-results :by #'cddr
                 :do (apply-edit cursor edit
                                 :after-step (lambda ()
                                               (when (eq mode :small-step)
                                                 (update-cache analyzer cache))))
                     (when (eq mode :big-step)
                       (update-cache analyzer cache))
                     (let ((input   (cons (buffer-string buffer)
                                          (format nil "~A mode, after edit ~D"
                                                  mode (1+ i))))
                           (results (update-cache analyzer cache)))
                       (are-results expected-results results :input input)))))
    (do-it :small-step) ; update cache after each item change
    (do-it :big-step))) ; update cache after each edit

(defmacro edits-cases (() &body cases)
  `(progn ,@(mapcar (lambda (case) `(edits-test-case ,@case)) cases)))

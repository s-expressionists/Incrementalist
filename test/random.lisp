(cl:in-package #:incrementalist.test)

(def-suite* :incrementalist.random
  :in :incrementalist)

;;; An input string generator that produces randomized nested
;;; expressions and a corresponding reader macro that randomly picks
;;; and permutes sub-expressions of those expressions.

(defstruct (cell (:constructor cell (path raw))
                 (:conc-name nil)
                 (:predicate nil)
                 (:copier nil))
  (path     (error "required") :read-only t)
  (raw      (error "required"))
  (location nil))

(defun gen-sub-expression
    (&key (sub-expression-count (5am:gen-integer :min 1 :max 5))
          (cst-children-count   (5am:gen-integer :min 1 :max 5)))
  (lambda ()
    (let ((symbols              '(a b c d e))
          (sub-expression-count (funcall sub-expression-count))
          (cst-children-count   (funcall cst-children-count))
          (infos                '()))
      (labels ((simple ()
                 (let ((symbol (pop symbols)))
                   (values (lambda (stream)
                             (write symbol :stream stream))
                           symbol)))
               (sub-expression (path)
                 (if (> (length path) 2)
                     (simple)
                     (case (random 2)
                       (0 (simple))
                       (1 (multiple-value-bind (inner-expression inner-raw)
                              (rec (append path '(first)))
                            (values (lambda (stream)
                                      (write-string "(" stream)
                                      (funcall inner-expression stream)
                                      (write-string ")" stream))
                                    `(,inner-raw)))))))
               (rec (path)
                 (multiple-value-bind (sub-expression raw)
                     (sub-expression path)
                   (let ((cell (cell path raw)))
                     (push cell infos)
                     (values (lambda (stream)
                               (let ((start (file-position stream))
                                     (end   (progn
                                              (funcall sub-expression stream)
                                              (file-position stream))))
                                 (setf (location cell) (cons start end))))
                             raw)))))
        (let ((writers (loop for i below sub-expression-count
                             collect (rec (list i)))))
          (list (lambda (stream)
                  (let ((firstp t))
                    (mapc (lambda (writer)
                            (if firstp
                                (setf firstp nil)
                                (write-char #\Space stream))
                            (funcall writer stream))
                          writers)))
                (map-into (make-list cst-children-count)
                          (lambda ()
                            (case (random 5)
                              ((0 1 2 3) (a:random-elt infos))
                              (4         (cons (a:random-elt infos)
                                               (a:random-elt infos))))))
                infos))))))

;;; Return a readtable which has a reader macro for
;;; #{ SUB-EXPRESSIONS } which reads SUB-EXPRESSIONS and repeatedly
;;; collects random sub-expressions of randomly selected elements
;;; (with duplicates) of SUB-EXPRESSIONS.
(defun augmented-readtable (sub-expressions)
  (let ((readtable (eclector.readtable:copy-readtable
                    eclector.reader:*readtable*)))
    (eclector.readtable:set-dispatch-macro-character
     readtable #\# #\{
     (lambda (stream char sub-char)
       (declare (ignore char sub-char))
       (let ((result (eclector.reader:read-delimited-list #\} stream t)))
         (labels ((select (path expression)
                    (destructuring-bind (&optional first . rest) path
                      (etypecase first
                        (null        expression)
                        ((eql first) (select rest (first expression)))
                        (integer     (select rest (nth first expression))))))
                  (expand (cell result)
                    (etypecase cell
                      (cons (destructuring-bind (car-cell . cdr-cell) cell
                              (cons (select (path car-cell) result)
                                    (select (path cdr-cell) result))))
                      (cell (select (path cell) result)))))
           (mapcar (a:rcurry #'expand result) sub-expressions)))))
    readtable))

(test rearranging-reader-macro
  "Test reading random nested lists that get randomly rearranged by a
reader macro."
  (5am:for-all ((writer-and-info (gen-sub-expression)))
    (destructuring-bind (writer sub-expressions infos) writer-and-info
      (declare (ignore infos))
      (let* ((input         (with-output-to-string (stream)
                              (write-string "#{ " stream)
                              (funcall writer stream)
                              (write-string " }" stream)))
             (readtable     (augmented-readtable sub-expressions))
             (initial-state `((*readtable* . ,readtable)))
             (result        (first (parse-result
                                    input
                                    :initial-reader-state initial-state))))
        (is-true (typep result 'inc:cons-wad))
        ;; Ensure that the structure, raw value and source information
        ;; of the CST corresponds to that of the input and reader
        ;; macro behavior.
        (labels ((equal/token (expected actual)
                   (typecase expected
                     (cons
                      (and (consp actual)
                           (equal/token (car expected) (car actual))
                           (equal/token (cdr expected) (cdr actual))))
                     (null
                      (null actual))
                     (symbol
                      (and (typep actual 'inc:symbol-token)
                           (string= (symbol-name expected)
                                    (inc:name actual))))))
                 (check (child sub-expression)
                   (etypecase sub-expression
                     (cons
                      (is-true (cst:consp child))
                      (check (cst:first child) (first sub-expression))
                      (let ((rest (rest sub-expression)))
                        (if (null rest)
                            (is-true (cst:null (cst:rest child)))
                            (check (cst:rest child) rest))))
                     (cell
                       (let ((expected-raw (raw sub-expression)))
                         (is (equal/token expected-raw (cst:raw child))))
                       (destructuring-bind (expected-start . expected-end)
                           (location sub-expression)
                         (is (eql expected-start (inc:start-column child)))
                         (is (eql expected-end   (inc:end-column child))))))))
          (check result sub-expressions))))))

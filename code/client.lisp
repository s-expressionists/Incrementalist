(cl:in-package #:incrementalist)

(defclass client (eclector.concrete-syntax-tree:definition-csts-mixin
                  eclector.concrete-syntax-tree:reference-csts-mixin
                  eclector.concrete-syntax-tree:cst-client)
  ;; TODO it would be nicer not to store the stream in the client like
  ;; this, but the method on make-expression-result needs the stream
  ;; and cannot access it in other ways.
  ((stream* :initarg :stream*
            :reader  stream*)))

;;; Feature expressions

(defmethod reader:check-feature-expression ((client             client)
                                            (feature-expression t))
  t)

(defmethod reader:evaluate-feature-expression ((client             client)
                                               (feature-expression t))
  nil)

;;; Read-time evaluation

(defmethod reader:evaluate-expression ((client client) (expression t))
  1)

;;; Token interpretation

(defmethod reader:call-reader-macro :around ((client       client)
                                             (input-stream t)
                                             (char         t)
                                             (readtable    t))
  (let ((values (multiple-value-list (call-next-method))))
    (typecase values
      (null
       (values))
      ((cons null null)
       (make-instance 'existing-symbol-token
                      :name         **nil-symbol-name**
                      :package-name **common-lisp-package-name**))
      (t
       (values-list values)))))

(defmethod reader:interpret-symbol-token ((client                    client)
                                          (input-stream              t)
                                          (token                     string)
                                          (position-package-marker-1 t)
                                          (position-package-marker-2 t))
  (multiple-value-bind (package-designator symbol-name)
      (cond ((null position-package-marker-1)
             (values *package* (intern-symbol-name token)))
            ((null position-package-marker-2)
             (values (if (= position-package-marker-1 0)
                         **keyword-package-name**
                         (intern-package-name
                          (subseq token 0 position-package-marker-1)))
                     (intern-symbol-name
                      (subseq token (1+ position-package-marker-1)))))
            (t
             (values (intern-package-name
                      (subseq token 0 position-package-marker-1))
                     (intern-symbol-name
                      (subseq token (1+ position-package-marker-2))))))
    (let ((package (find-package package-designator)))
      (macrolet ((make-token (class package-name symbol-name)
                   `(let ((package-name (intern-package-name ,package-name))
                          (symbol-name  (intern-symbol-name ,symbol-name)))
                      (make-instance
                       ',class :package-name     package-name
                               :package-marker-1 position-package-marker-1
                               :package-marker-2 position-package-marker-2
                               :name             symbol-name))))
        (if (null package)
            (make-token non-existing-package-symbol-token
                        package-designator
                        symbol-name)
            (multiple-value-bind (symbol status)
                (find-symbol symbol-name package)
              (if (null status)
                  (make-token non-existing-symbol-token
                              (cl:package-name package)
                              symbol-name)
                  (make-token existing-symbol-token
                              (cl:package-name (symbol-package symbol))
                              (cl:symbol-name symbol)))))))))

(defmethod reader:interpret-symbol ((client            client)
                                    (input-stream      t)
                                    (package-indicator null)
                                    (symbol-name       t)
                                    (internp           t))
  (make-instance 'uninterned-symbol-token :name symbol-name))

;;; Source position

(defmethod eclector.base:source-position ((client client) (stream buffer-stream))
  (cons (line-number stream) (item-number stream)))

(defmethod eclector.base:make-source-range ((client client) (start t) (end t))
  (cons start end))

;;; Result construction

(defmethod eclector.parse-result:make-skipped-input-result
    ((client client) (stream t) (reason t) (source t))
  (etypecase reason
    ((cons (eql :line-comment))
     (let* ((semicolon-count (cdr reason))
            (text-wads       (make-text-wads
                              stream source
                              :start-column-offset semicolon-count)))
       (wad-with-children 'semicolon-comment-wad stream source text-wads
                          :semicolon-count semicolon-count)))
    ((eql :block-comment)
     (let ((words (make-text-wads stream source :start-column-offset 2
                                                :end-column-offset   -2)))
       (wad-with-children 'block-comment-wad stream source words)))
    ((eql :reader-macro)
     (basic-wad 'reader-macro-wad stream source))
    ((eql *read-suppress*)
     (basic-wad 'read-suppress-wad stream source))
    ((cons (eql :sharpsign-plus))
     (basic-wad 'sharpsign-plus-wad stream source :expression (cdr reason)))
    ((cons (eql :sharpsign-minus))
     (basic-wad 'sharpsign-minus-wad stream source :expression (cdr reason)))))

(defmethod eclector.parse-result:make-expression-result
    ((client client) (result symbol-token) (children t) (source t))
  (if (and (null children)
           (not (eq (package-name result) **common-lisp-package-name**)))
      (let* ((stream (stream* client))
             (words  (make-text-wads stream source :min-length 2)))
        (if (null words)
            (call-next-method)
            (wad-with-children
             'atom-wad-with-extra-children stream source words :raw result)))
      (call-next-method)))

(defmethod eclector.parse-result:make-expression-result
    ((client client) (result string) (children t) (source t))
  (if (null children)
      (let* ((stream (stream* client))
             (words  (make-text-wads stream source
                                     :start-column-offset 1
                                     :end-column-offset   -1)))
        (if (null words)
            (call-next-method)
            (wad-with-children
             'atom-wad-with-extra-children stream source words :raw result)))
      (call-next-method)))

(defun adjust-result-class (cst new-class stream source &rest extra-initargs)
  (declare (dynamic-extent extra-initargs))
  (destructure-source (start-line start-column end-line end-column) source
    (let* ((line-number    (line-number stream))
           (max-line-width (compute-max-line-width
                            stream start-line line-number '())))
      (apply #'change-class cst new-class
             :source              nil   ; obsolete; free memory
             :cache               *cache*
             :relative-p          nil
             :start-line          start-line
             :height              (- end-line start-line)
             :start-column        start-column
             :end-column          end-column
             :max-line-width      max-line-width
             :absolute-start-line start-line
             extra-initargs))))

(defun adjust-result (cst new-class stream source &rest extra-initargs)
  (declare (dynamic-extent extra-initargs))
  (assert (not (null source)))
  (let ((result (if extra-initargs
                    (apply #'adjust-result-class cst new-class stream source
                           extra-initargs)
                    (adjust-result-class cst new-class stream source))))
    ;; After the optional `add-extra-children', the sequence of
    ;; children of RESULT is final. Perform two adjustments on this
    ;; child sequence: 1. make the children relative (they are
    ;; guaranteed to be absolute) 2. set up the parent-child and
    ;; sibling relations.
    (make-children-relative-and-set-family-relations result)))

;;; Return a list of elements of CHILDREN that are of type `cst:cst'
;;; with the following properties:
;;; 1. The returned list is always `equal' to
;;;    (remove-if-not (a:of-type 'cst:cst) children)
;;; 2. If all elements of CHILDREN are of type `cst:cst' then the
;;;    returned list is `eq' to CHILDREN.
;;; The second property avoids consing and also informs subsequent
;;; processing steps about whether there are any non-CST children.
(defun collect-cst-children (children)
  (loop :with any-extra-p = nil
        :for i :from 0
        :for child :in children
        :for cstp = (typep child 'cst:cst)
        :if (and (not cstp) (not any-extra-p))
          :append (subseq children 0 i) :into cst-children
          :and :do (setf any-extra-p t)
        :else :if (and cstp any-extra-p)
          :collect child :into cst-children
        :finally (return (if any-extra-p cst-children children))))

;;; Note: CHILDREN are already ordered according to their location in
;;; input text.
(defmethod eclector.parse-result:make-expression-result
    ((client client) (result t) (children t) (source t))
  (let ((stream (stream* client)))
    ;; In case we call `cst:reconstruct', we may "consume" a child wad
    ;; that is still on the residue or suffix without performing the
    ;; corresponding recursive `read-maybe-nothing' call. As a
    ;; workaround, consume any such wads here.
    (cached-wad stream)
    ;; Separate CHILDREN into children of type `cst:cst' and "extra"
    ;; children. Extra children arise mainly due to comments and other
    ;; skipped input which are represented as `wad's which are not of
    ;; type `cst:cst'.
    (let* ((cst-children (if (null children)
                             '()
                             (collect-cst-children children)))
           (cst          (if (eq cst-children children)
                             (call-next-method) ; possibly faster
                             (call-next-method
                              client result cst-children source)))
           (consp        (typep cst 'cst:cons-cst))
           ;; Check whether the list of wad children obtained based on
           ;; the CST structure already matches the list of children.
           (simplep      (or (null children)
                             (and consp
                                  (eq cst-children children)
                                  (block nil
                                    (let ((remaining children))
                                      (map-children
                                       (lambda (child)
                                         (when (not (eq child (pop remaining)))
                                           (return nil)))
                                       cst)
                                      (null remaining))))))
           (new-class    (cond ((and (not consp) simplep)
                                'atom-wad)
                               ((not consp)
                                'atom-wad-with-extra-children)
                               (simplep
                                'cons-wad)
                               (t
                                'cons-wad-with-extra-children))))
      ;; Call the next method to obtain a result, CST, of type `cst:cst'
      ;; which contains RESULT in its raw slot.
      ;;
      ;; Two properties of CST will be adjusted:
      ;; 1. Depending on whether the result is a `cst:cons-cst' or a
      ;;    `cst:atom-cst', select either `cons-wad' or `atom-wad' as
      ;;    the new class for CST.
      ;; 2. In case there are "extra" children or "orphan" CST children,
      ;;    add those to the result.
      (if simplep
          (adjust-result cst new-class stream source)
          (adjust-result cst new-class stream source :children children)))))

(defmethod eclector.parse-result:make-expression-result
    ((client   client)
     (result   eclector.parse-result:definition)
     (children t)
     (source   t))
  (let ((cst    (call-next-method))
        (stream (stream* client)))
    (adjust-result cst 'labeled-object-definition-wad stream source
                   :children children)))

(defmethod eclector.parse-result:make-expression-result
    ((client   client)
     (result   eclector.parse-result:reference)
     (children t)
     (source   t))
  (let ((cst    (call-next-method))
        (stream (stream* client)))
    (adjust-result cst 'labeled-object-reference-wad stream source)))

;;; S-expression generation

(flet ((make-form (symbol-name package-name &rest rest)
         (let ((head (make-instance 'existing-symbol-token
                                    :name         symbol-name
                                    :package-name package-name)))
           (list* head rest))))

  (defmethod eclector.reader:wrap-in-quote ((client client) (material t))
    (make-form "QUOTE" **common-lisp-package-name** material))

  (defmethod eclector.reader:wrap-in-quasiquote ((client client) (form t))
    (make-form "QUASIQUOTE" "KEYWORD" form))

  (defmethod eclector.reader:wrap-in-unquote ((client client) (form t))
    (make-form "UNQUOTE" "KEYWORD" form))

  (defmethod eclector.reader:wrap-in-unquote-splicing ((client client) (form t))
    (make-form "UNQUOTE-SPLICING" "KEYWORD" form))

  (defmethod eclector.reader:wrap-in-function ((client client) (name t))
    (make-form "FUNCTION" **common-lisp-package-name** name)))

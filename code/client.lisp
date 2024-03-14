(cl:in-package #:incrementalist)

(defclass client (eclector.parse-result:parse-result-client)
  ;; TODO it would be nicer not to store the stream in the client like
  ;; this, but the method on make-expression-result needs the stream
  ;; and cannot access it in other ways.
  ((stream* :initarg :stream*
            :reader  stream*)))

;;; Feature expressions

(defmethod reader:check-feature-expression ((client client) (feature-expression t))
  t)

(defmethod reader:evaluate-feature-expression ((client client) (feature-expression t))
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
       (make-instance 'existing-symbol-token :name         "NIL"
                                             :package-name "COMMON-LISP"))
      (t
       (values-list values)))))

(defmethod reader:interpret-symbol-token
    ((client client) input-stream token position-package-marker-1 position-package-marker-2)
  (multiple-value-bind (package-designator symbol-name)
      (cond ((null position-package-marker-1)
             (values *package* token))
            ((null position-package-marker-2)
             (values (if (= position-package-marker-1 0)
                         "KEYWORD"
                         (subseq token 0 position-package-marker-1))
                     (subseq token (1+ position-package-marker-1))))
            (t
             (values (subseq token 0 position-package-marker-1)
                     (subseq token (1+ position-package-marker-2)))))
    (let ((package (find-package package-designator)))
      (if (null package)
          (make-instance 'non-existing-package-symbol-token
                         :package-name package-designator
                         :package-marker-1 position-package-marker-1
                         :package-marker-2 position-package-marker-2
                         :name symbol-name)
          (multiple-value-bind (symbol status)
              (find-symbol symbol-name package)
            (if (null status)
                (make-instance 'non-existing-symbol-token
                               :package-name (cl:package-name package)
                               :package-marker-1 position-package-marker-1
                               :package-marker-2 position-package-marker-2
                               :name symbol-name)
                (make-instance 'existing-symbol-token
                               :package-name (cl:package-name (symbol-package symbol))
                               :package-marker-1 position-package-marker-1
                               :package-marker-2 position-package-marker-2
                               :name (cl:symbol-name symbol))))))))

;;; Source position

(defmethod eclector.base:source-position ((client client) (stream buffer-stream))
  (cons (line-number stream) (item-number stream)))

(defmethod eclector.base:make-source-range ((client client) (start t) (end t))
  (cons start end))

;;; Result construction

(defmacro destructure-source ((start-line-var start-column-var
                               end-line-var   end-column-var)
                              source &body body)
  `(destructuring-bind ((,start-line-var . ,start-column-var)
                        . (,end-line-var . ,end-column-var))
       ,source
     (declare (type alexandria:array-index
                    ,start-line-var ,start-column-var
                    ,end-line-var   ,end-column-var))
     ,@body ))

(defmacro basic-wad (class stream source &rest extra-initargs)
  (alexandria:once-only (stream)
    (alexandria:with-unique-names (start-line start-column end-line end-column
                                   line-number max-line-width)
      `(destructure-source (,start-line ,start-column ,end-line ,end-column)
           ,source
         (let* ((,line-number    (line-number ,stream))
                (,max-line-width (compute-max-line-width
                                  ,stream ,start-line ,line-number '())))
           (make-instance ,class :cache               *cache*
                                 :relative-p          nil
                                 :absolute-start-line ,start-line
                                 :start-line          ,start-line
                                 :height              (- ,end-line ,start-line)
                                 :start-column        ,start-column
                                 :end-column          ,end-column
                                 :max-line-width      ,max-line-width
                                 ,@extra-initargs))))))

(defmacro wad-with-children (class stream source children &rest extra-initargs)
  (alexandria:once-only (children)
    `(let ((result (basic-wad ,class ,stream ,source
                              :children ,children
                              ,@extra-initargs)))
       (set-family-relations-of-children result)
       (make-relative ,children (start-line result))
       result)))

(defun make-word-wads (stream source
                       &key (start-column-offset 0)
                            (end-column-offset   0 end-column-offset-p))
  (destructure-source (start-line start-column end-line end-column*) source
    (let* ((cache             (cache stream))
           (word              (make-array 0 :element-type 'character
                                            :adjustable   t
                                            :fill-pointer 0))
           (word-start-column (+ start-column start-column-offset))
           (words             '()))
      (flet ((terminatingp (character)
               (let ((spacep       (whitespacep character))
                     (punctuationp (punctuationp character)))
                 (values (or spacep punctuationp) punctuationp)))
             (commit (line column checkp)
               (when (and (plusp (length word))
                          (notany #'digit-char-p word)
                          (notevery #'punctuationp word))
                 (let ((source      (cons (cons line word-start-column)
                                          (cons line column)))
                       (misspelledp (and checkp
                                         (null (spell:english-lookup word)))))
                   (push (basic-wad 'word-wad stream source
                                    :misspelled misspelledp)
                         words)))
               (setf (fill-pointer word) 0
                     word-start-column   column)))
        (loop for line from start-line to (if (zerop end-column*)
                                              (1- end-line)
                                              end-line)
              for contents of-type simple-string = (line-contents cache line)
              do (loop with end-column = (if (= line end-line)
                                             (+ end-column*
                                                (if end-column-offset-p
                                                    end-column-offset
                                                    0))
                                             (length contents))
                       for column from word-start-column below end-column
                       for character = (aref contents column)
                       for (terminatingp punctuationp)
                          = (multiple-value-list (terminatingp character))
                       do (cond ((not terminatingp)
                                 (vector-push-extend character word))
                                (punctuationp
                                 (commit line column t)
                                 (vector-push-extend character word)
                                 (commit line (1+ column) nil))
                                (t
                                 (commit line column t)
                                 (incf word-start-column)))
                       finally (commit line column t))
                 (setf word-start-column 0))
        (nreverse words)))))

(defmethod eclector.parse-result:make-skipped-input-result
    ((client client) (stream t) (reason t) (source t))
  (etypecase reason
    ((cons (eql :line-comment))
     ;; Eclector returns the beginning of the following line as the
     ;; end of the comment.  But we want it to be the end of the
     ;; same line.  But I don't know how to do it correctly (yet).
     (let* ((semicolon-count (cdr reason))
            (words           (make-word-wads
                              stream source
                              :start-column-offset semicolon-count)))
       (wad-with-children 'semicolon-comment-wad stream source words
                          :semicolon-count semicolon-count)))
    ((eql :block-comment)
     (let ((words (make-word-wads stream source :start-column-offset 2
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
  (if (and (null children) (not (string= (package-name result) "COMMON-LISP")))
      (let ((words (make-word-wads (stream* client) source)))
        (if (null words)
            (call-next-method)
            (wad-with-children 'expression-wad (stream* client) source words
                               :expression result)))
      (call-next-method)))

(defmethod eclector.parse-result:make-expression-result
    ((client client) (result string) (children t) (source t))
  (if (null children)
      (let ((words (make-word-wads (stream* client) source)))
        (if (null words)
            (call-next-method)
            (wad-with-children 'expression-wad (stream* client) source words
                               :expression result)))
      (call-next-method)))

(defmethod eclector.parse-result:make-expression-result
    ((client client) (result t) (children t) (source t))
  (wad-with-children 'expression-wad (stream* client) source children
                     :expression result))

(defmethod eclector.parse-result:make-expression-result
    ((client   client)
     (result   eclector.parse-result:definition)
     (children t)
     (source   t))
  (let ((stream (stream* client))
        (labeled-object (eclector.parse-result:labeled-object result)))
    (multiple-value-bind (state object parse-result)
        (reader:labeled-object-state client labeled-object)
      (declare (ignore state))
      (assert (member parse-result children))
      (wad-with-children 'labeled-object-definition-wad stream source children
                         :expression object))))

(defmethod eclector.parse-result:make-expression-result
    ((client   client)
     (result   eclector.parse-result:reference)
     (children t)
     (source   t))
  (let* ((stream (stream* client))
         (labeled-object (eclector.parse-result:labeled-object result))
         (object (nth-value
                  1 (reader:labeled-object-state client labeled-object))))
    (basic-wad 'labeled-object-reference-wad stream source
               :expression object)))
;;; S-expression generation

(flet ((make-form (symbol-name package-name &rest rest)
         (let ((head (make-instance 'existing-symbol-token
                                    :name         symbol-name
                                    :package-name package-name)))
           (list* head rest))))

  (defmethod eclector.reader:wrap-in-quote ((client client) (material t))
    (make-form "QUOTE" "COMMON-LISP" material))

  (defmethod eclector.reader:wrap-in-quasiquote ((client client) (form t))
    (make-form "QUASIQUOTE" "KEYWORD" form))

  (defmethod eclector.reader:wrap-in-unquote ((client client) (form t))
    (make-form "UNQUOTE" "KEYWORD" form))

  (defmethod eclector.reader:wrap-in-unquote-splicing ((client client) (form t))
    (make-form "UNQUOTE-SPLICING" "KEYWORD" form))

  (defmethod eclector.reader:wrap-in-function ((client client) (name t))
    (make-form "FUNCTION" "COMMON-LISP" name)))

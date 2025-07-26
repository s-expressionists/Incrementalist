(cl:in-package #:incrementalist)

(defclass token () ())

(defclass symbol-token (token)
  ;; TODO: does this, or the symbol itself need a reference to the environment?
  ;; maybe not. maybe could make it so that `symbol-name' and `symbol-package' work for our symbol classes even if `parcl:*client*' is `nil'. we would have to bind `parcl:*client*' to `nil' be default
  ;; Then the package objects could have references to their environments?
  ;; No, that doesn't work because the status of a symbol can be different in different environments
  ;; So does the symbol or the token itself need a reference to the environment after all?
  ((%symbol :initarg  :symbol
            :type     (not null)     ; TODO: (satisfies parcl:symbolp)
            :reader   symbol)))

(defun maybe-truncate-string (thing &key (limit 32))
  (cond ((not (stringp thing))
         thing)
        ((<= (length thing) limit)
         thing)
        (t
         (concatenate 'string (subseq thing 0 (1- limit)) "…"))))

(defmethod print-object ((object symbol-token) stream)
  (let* ((symbol  (symbol object))
         (package (pkg:symbol-package symbol)))
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~@[[~A]~]~:[~;:~]~:[~;:~]~A"
              (when package
                (maybe-truncate-string (pkg:package-name package)))
              (package-marker-1 object)
              (package-marker-2 object)
              (maybe-truncate-string (pkg:symbol-name symbol))))))

(defclass uninterned-symbol-token (symbol-token)
  ())

(defmethod package-marker-1 ((symbol-token uninterned-symbol-token))
  nil)

(defmethod package-marker-2 ((symbol-token uninterned-symbol-token))
  nil)

;; TODO: make two sub-classes of `interned-symbol-token', with and without package-marker slots
;; TODO: store both package-markers in one slot; since adjacent second marker needs only one bit
(defclass interned-symbol-token (symbol-token)
  ((%package-marker-1 :initarg  :package-marker-1
                      :type     (or null integer)
                      :reader   package-marker-1
                      :initform nil)
   (%package-marker-2 :initarg  :package-marker-2
                      :type     (or null integer)
                      :reader   package-marker-2
                      :initform nil)))

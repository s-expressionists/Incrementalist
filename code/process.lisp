(cl:in-package #:incrementalist)

(defun maybe-set-read-base (client expression)
  (if (and (typep expression '(cons symbol-token
                                    (cons symbol-token
                                          (cons t null))))
           (let* ((symbol  (symbol (first expression)))
                  (package (pkg:symbol-package symbol)))
             (and package
                  (equal (pkg:package-name package) "COMMON-LISP")
                  (equal (pkg:symbol-name symbol) "SETF")))
           (let* ((symbol  (symbol (second expression)))
                  (package (pkg:symbol-package symbol)))
             (and package
                  (equal (pkg:package-name package) "COMMON-LISP")
                  (equal (pkg:symbol-name symbol) "*READ-BASE*"))))
      (let* ((form   (subst `(eclector.reader:state-value ,client '*read-base*)
                            '*read-base*
                            `(setf *read-base* ,(third expression))))
             (result (eval form)))
        (dbg:log :eval "Expression ~S~%~2@T-> ~S~%~2@T-> ~S~%"
                 expression form result)
        (values result t))))

(defun maybe-change-package (client wad)
  (let (second)
    (when (and (typep wad 'cst:cst)
               (cst:consp wad)
               (let ((first (cst:first wad)))
                 (and (cst:atom first)
                      (let ((raw (cst:raw first)))
                        (and (typep raw 'symbol-token)
                             (let ((symbol  (symbol raw)))
                               (and (string= (pkg:symbol-name symbol) "IN-PACKAGE")
                                    (let ((package (pkg:symbol-package symbol)))
                                      (and package
                                           (string= (pkg:package-name package) "COMMON-LISP")))))))))
               (let ((rest (cst:rest wad)))
                 (and (cst:consp rest)
                      (cst:null (cst:rest rest))
                      (typep (setf second (cst:first rest)) 'cst:cst)
                      (cst:atom second)
                      (let ((raw (cst:raw second)))
                        (typep raw '(or string symbol-token))))))
      (setf (reader:state-value client '*package*)
            (let ((raw (cst:raw (cst:first (cst:rest wad)))))
              ;; TODO: set current package around `find-package' calls for package-local nicknames
              (pkg::find-undeleted-package-or-error
               pkg:*client* (typecase raw
                              (string raw)
                              (t      (pkg:symbol-name (symbol raw)))))))
      t)))

(defun apply-semantics (client wad)
  ;; HACK: we support setting `*package*' as a special case for
  ;; testing
  (dep:with-cell-dependencies (add-dependencies)
    (when (maybe-change-package client wad)
      (add-dependencies wad))))

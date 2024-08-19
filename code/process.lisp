(cl:in-package #:incrementalist)

(defun maybe-set-read-base (client expression)
  (if (and (typep expression '(cons existing-symbol-token
                               (cons existing-symbol-token
                                (cons t null))))
           (and (equal (package-name (first expression))
                       **common-lisp-package-name**)
                (equal (name (first expression)) "SETF"))
           (and (equal (package-name (second expression))
                       **common-lisp-package-name**)
                (equal (name (second expression)) "*READ-BASE*")))
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
                      (typep (cst:raw first) 'existing-symbol-token)
                      (string= (package-name (cst:raw first)) "COMMON-LISP")
                      (let ((raw (cst:raw first)))
                        (string= (typecase raw
                                   (string raw)
                                   (t      (name raw)))
                                 "IN-PACKAGE"))))
               (let ((rest (cst:rest wad)))
                 (and (cst:consp rest)
                      (cst:null (cst:rest rest))
                      (typep (setf second (cst:first rest)) 'cst:cst)
                      (cst:atom second)
                      (let ((raw (cst:raw second)))
                        (typep raw '(or string symbol-token))))))
      (setf (reader:state-value client '*package*)
            (let ((raw (cst:raw (cst:first (cst:rest wad)))))
              (typecase raw
                (string raw)
                (t      (name raw)))))
      t)))

(defun apply-semantics (client wad)
  ;; HACK: we support setting `*package*' as a special case for
  ;; testing
  (dep:with-cell-dependencies (add-dependencies)
    (when (maybe-change-package client wad)
      (add-dependencies wad))))

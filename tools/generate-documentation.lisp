(defun filtered-children (node test)
  (let ((children (sb-mop:class-direct-subclasses node)))
    (if (null test)
        children
        (loop :for child :in children
              :if (funcall test child)
                :collect child
              :else
                :append (filtered-children child test)))))

(defun format-class-graph (stream root &key test)
  (format stream "@example~%")
  (let ((utilities.print-tree:*use-unicode?* nil))
    (utilities.print-tree:print-tree
     stream root
     (utilities.print-tree:make-node-printer
      (lambda (stream depth object)
        (declare (ignore depth))
        (let* ((class-name   (class-name object))
               (package-name (package-name (symbol-package class-name))))
          (format stream "@link{Class ~(~A~)|~(~A~),~:*~(~A~)}"
                  package-name class-name)))
      nil (lambda (node)
            (let ((children (filtered-children node test)))
              (sort (copy-list children) #'string< :key #'class-name))))))
  (format stream "~&@end example~%"))

(destructuring-bind (wad-class-hierarchy-file
                     token-class-hierarchy-file)
    (rest sb-ext:*posix-argv*)

  (with-open-file (stream token-class-hierarchy-file
                          :direction         :output
                          :if-does-not-exist :create
                          :if-exists         :supersede)
    (format-class-graph stream (find-class 'incrementalist::token)))

  (with-open-file (stream wad-class-hierarchy-file
                          :direction         :output
                          :if-does-not-exist :create
                          :if-exists         :supersede)
    (format-class-graph
     stream (find-class 'incrementalist::basic-wad)
     :test (lambda (class)
             (not (find (class-name class)
                        '(incrementalist::atom-wad-with-extra-children
                          incrementalist::cons-wad-with-extra-children)))))))

(cl:in-package #:incrementalist)

(defun report (message &rest arguments)
  (apply #'format *trace-output* message arguments))

(defun check-wad (wad parent)
  (unless (eq (parent wad) parent)
    (report "a: Parent of ~s is ~s rather than ~s~%"
            wad (parent wad) parent))
  (let ((children (children wad)))
    (unless (null children)
      (unless (null (left-sibling (first children)))
        (report "a: Left sibling of ~s is ~s rather than NIL~%"
                (first children) (left-sibling (first children))))
      (unless (null (right-sibling (first (last children))))
        (report "a: Right sibling of ~s is ~s rather than NIL~%"
                (first (last children))
                (right-sibling (first (last children)))))
      (loop for (left right) on children
            until (null right)
            do (unless (eq (right-sibling left) right)
                 (report "a: Right sibling of ~s is ~s rather than ~s~%"
                         left (right-sibling left) right))
               (unless (eq (left-sibling right) left)
                 (report "a: Left sibling of ~s is ~s rather than ~s~%"
                         right (left-sibling right) left))))
    (loop for child in children
          do (check-wad child wad))))

(defun check-wad-graph (cache)
  (let ((prefix (prefix cache))
        (suffix (suffix cache)))
    (unless (null prefix)
      (unless (null (left-sibling (first (last prefix))))
        (report "b: Left sibling of last (~s) is ~s rather than NIL.~s"
                (first (last prefix)) (left-sibling (first (last prefix)))))
      (loop for (right left) on prefix
            until (null left)
            do (unless (eq (left-sibling right) left)
                 (report "b: Left sibling of ~s is ~s rather than ~s~%"
                         right (left-sibling right) left))
               (unless (eq (right-sibling left) right)
                 (report "b: Right sibling of ~s is ~s rather than ~s~%"
                         left (right-sibling left) right))))
    (unless (null suffix)
      (unless (null (right-sibling (first (last suffix))))
        (report "c: Right sibling of last (~s) is ~s rather than NIL.~%"
                (first (last suffix))
                (right-sibling (first (last suffix)))))
      (loop for (left right) on suffix
            until (null right)
            do (unless (eq (left-sibling right) left)
                 (report "c: Left sibling of ~s is ~s rather than ~s~%"
                         right (left-sibling right) left))
               (unless (eq (right-sibling left) right)
                 (report "c: Right sibling of ~s is ~s rather than ~s~%"
                         left (right-sibling left) right))))
    (if (null prefix)
        (if (null suffix)
            nil
            (unless (null (left-sibling (first suffix)))
              (report "d: Left sibling of ~s is ~s rather than NIL~%"
                      (first suffix) (left-sibling (first suffix)))))
        (if (null suffix)
            (unless (null (right-sibling (first prefix)))
              (report "d: Right sibling of ~s is ~s rather than NIL~%"
                      (first prefix) (right-sibling (first prefix))))
            (progn
              (unless (eq (right-sibling (first prefix)) (first suffix))
                (report "e: Right sibling of ~s is ~s rather than ~s~%"
                        (first prefix)
                        (right-sibling (first prefix))
                        (first suffix)))
              (unless (eq (left-sibling (first suffix)) (first prefix))
                (report "e: Left sibling of ~s is ~s rather than ~s~%"
                        (first suffix)
                        (left-sibling (first suffix))
                        (first prefix))))))
    (loop for wad in prefix do (check-wad wad nil))
    (loop for wad in suffix do (check-wad wad nil))))

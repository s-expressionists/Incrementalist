(cl:in-package #:incrementalist)

;;; This function is used by the INDENT-LINE command.  Given a line
;;; number, it returns a wad that starts on a particular line number,
;;; such that no other wad starts to the left of it on the same line.
;;; If no wad can be found that fulfills this requirement, then NIL is
;;; returned.
;;;
;;; This function probably doesn't have to be particularly efficient,
;;; so we do the simplest possible thing, namely we move all wads to
;;; the prefix (which the line numbers are absolute), and then we scan
;;; the prefix from the beginning.

(defun search-wad (wad start-line-number line-number)
  (if (= start-line-number line-number)
      wad
      (loop for child in (children wad)
            for absolute-line-number = start-line-number
              then (+ absolute-line-number (start-line child))
            when (<= absolute-line-number
                     line-number
                     (+ absolute-line-number (height child)))
              return (search-wad child absolute-line-number line-number))))

(defmethod find-wad-beginning-line ((cache cache) (line-number integer))
  (loop until (null (suffix cache))
        do (suffix-to-prefix cache))
  (loop for wad in (reverse (prefix cache))
        for start-line = (start-line wad)
        when (<= start-line line-number (+ start-line (height wad)))
          return (search-wad wad start-line line-number)))

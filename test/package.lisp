(cl:defpackage #:incrementalist.test
  (:use
   #:cl)

  (:local-nicknames
   (#:a   #:alexandria)

   (#:b   #:text.editor-buffer)

   (#:inc #:incrementalist)
   (#:dep #:incrementalist.dependencies))

  (:import-from #:fiveam
   #:def-suite
   #:def-suite*
   #:in-suite
   #:test
   #:is
   #:is-true
   #:is-false
   #:signals
   #:finishes)

  (:export
   #:run-tests))

(cl:in-package #:incrementalist.test)

(def-suite :incrementalist)

(defun run-tests ()
  (fiveam:run! :incrementalist))

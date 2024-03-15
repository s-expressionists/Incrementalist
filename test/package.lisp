(cl:defpackage #:incrementalist.test
  (:use
   #:cl)

  (:local-nicknames
   (#:a   #:alexandria)

   (#:inc #:incrementalist))

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

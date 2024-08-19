(defsystem "incrementalist"
  :description "Incremental parsing of Common Lisp code represented as a Cluffer buffer."
  :license     "BSD" ; see COPYING file
  :author      ("Robert Strandh"
                #1="Jan Moringen <jmoringe@techfak.uni-bielefeld.de>")
  :maintainer  #1#

  :version     (:read-file-form "data/version-string.sexp")
  :depends-on  ("trivial-gray-streams"
                "cluffer"
                "flexichain"
                "concrete-syntax-tree"
                "eclector"
                "eclector-concrete-syntax-tree"
                "spell")

  :components  ((:module     "debug"
                 :pathname   "code/debug"
                 :components ((:file "package")
                              (:file "log")
                              (:file "invariant"
                               :if-feature :incrementalist-debug)))

                (:module     "dependencies"
                 :pathname   "code/dependencies"
                 :depends-on ("debug")
                 :serial     t
                 :components ((:file "package")
                              (:file "protocol")
                              (:file "debug-types"
                               :if-feature :incrementalist-debug)
                              (:file "dependencies")
                              ;; Debugging
                              (:file "extra-assertions"
                               :if-feature :incrementalist-debug)))

                (:module     "code"
                 :depends-on ("debug" "dependencies")
                 :serial     t
                 :components ((:file "package")
                              (:file "utilities")
                              (:file "protocol")
                              ;; Model
                              (:file "token")
                              (:file "wad")
                              ;; Cache
                              (:file "cache")
                              (:file "buffer-stream")
                              (:file "analyzer")
                              (:file "text")
                              (:file "client")
                              (:file "dependencies")
                              (:file "read")
                              (:file "process")
                              (:file "update-cache")
                              ;; Queries
                              (:file "find-wad-beginning-line")
                              (:file "find-wad-containing-position")
                              (:file "mapwad")
                              ;; Debugging
                              (:file "extra-assertions"
                               :if-feature :incrementalist-debug))))

  :in-order-to ((test-op (test-op "incrementalist/test"))))

(defsystem "incrementalist/test"
  :description "Tests for the incrementalist system."
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "data/version-string.sexp")
  :depends-on  (;; Utilities
                "utilities.print-tree"
                ;; Test framework
                "fiveam"
                ;; Code under test
                "incrementalist")

  :components  ((:module     "test"
                 :serial     t
                 :components ((:file "package")
                              ;; Test utilities
                              (:file "utilities")
                              (:file "code-reading-utilities")
                              ;; Model
                              (:file "wad")
                              ;; Cache
                              (:file "dependencies")
                              ;; Queries
                              (:file "find-wad-containing-position")
                              ;; Other tests
                              (:file "test")
                              (:file "random")
                              (:file "random-dependencies")
                              (:file "read-code")
                              (:file "regressions")
                              (:file "performance"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:incrementalist.test '#:run-tests)))

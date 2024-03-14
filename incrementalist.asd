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

  :components  ((:module "code"
                 :serial      t
                 :components ((:file "package")
                              (:file "utilities")
                              (:file "protocol")
                              ;; Model
                              (:file "token")
                              (:file "wad")
                              (:file "check-wad-graph")
                              ;; Cache
                              (:file "cache")
                              (:file "buffer-stream")
                              (:file "analyzer")
                              (:file "client")
                              (:file "parse")
                              (:file "read-forms")
                              (:file "update-cache")
                              ;; Queries
                              (:file "find-wad-beginning-line")
                              (:file "find-wad-containing-position")
                              (:file "mapwad"))))

  :in-order-to ((test-op (test-op "incrementalist/test"))))

(defsystem "incrementalist/test"
  :description "Tests for the incrementalist system."
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "data/version-string.sexp")
  :depends-on  ("fiveam"

                "incrementalist")

  :components  ((:module     "test"
                 :serial     t
                 :components ((:file "package")
                              ;; Test utilities
                              (:file "utilities")
                              ;; Model
                              (:file "wad")
                              ;; Tests
                              (:file "test"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:incrementalist.test '#:run-tests)))

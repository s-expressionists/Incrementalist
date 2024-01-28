(cl:in-package #:asdf-user)

(defsystem "incrementalist"
  :depends-on ("trivial-gray-streams"
               "cluffer"
               "flexichain"
               "eclector"
               "spell")
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "wad")
   (:file "buffer-stream")
   (:file "cache")
   (:file "analyzer")
   (:file "token")
   (:file "client")
   (:file "parse")
   (:file "read-forms")
   (:file "update-cache")
   (:file "find-wad-beginning-line")
   (:file "find-wad-containing-position")
   (:file "mapwad")
   (:file "check-wad-graph")))

(cl:in-package #:asdf-user)

(defsystem "incrementalist"
  :license "BSD" ; see COPYING file
  :depends-on ("trivial-gray-streams"
               "cluffer"
               "flexichain"
               "eclector"
               "spell")
  :serial t
  :components
  ((:file "packages"
    :pathname "code/packages")
   (:file "utilities"
    :pathname "code/utilities")
   (:file "wad"
    :pathname "code/wad")
   (:file "buffer-stream"
    :pathname "code/buffer-stream")
   (:file "cache"
    :pathname "code/cache")
   (:file "analyzer"
    :pathname "code/analyzer")
   (:file "token"
    :pathname "code/token")
   (:file "client"
    :pathname "code/client")
   (:file "parse"
    :pathname "code/parse")
   (:file "read-forms"
    :pathname "code/read-forms")
   (:file "update-cache"
    :pathname "code/update-cache")
   (:file "find-wad-beginning-line"
    :pathname "code/find-wad-beginning-line")
   (:file "find-wad-containing-position"
    :pathname "code/find-wad-containing-position")
   (:file "mapwad"
    :pathname "code/mapwad")
   (:file "check-wad-graph"
    :pathname "code/check-wad-graph")))

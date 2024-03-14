(cl:defpackage #:incrementalist
  (:use
   #:common-lisp)

  (:local-nicknames
   (#:reader    #:eclector.reader)
   (#:readtable #:eclector.readtable)
   (#:stream    #:trivial-gray-streams)
   (#:flx       #:flexichain))

  (:shadow
   #:condition ; reader of `error-wad'
   #:package-name
   #:symbol-name)

  ;; Tokens
  (:export
   #:token ; class

   #:symbol-token ; class
   #:package-marker-1 ; readers
   #:package-marker-2
   #:package-name
   #:name

   #:non-existing-package-symbol-token ; classes
   #:non-existing-symbol-token
   #:existing-symbol-token)

  ;; Wads
  (:export
   #:map-children ; children protocol
   #:children

   #:parent ; family relations protocol
   #:left-sibling
   #:right-sibling

   #:absolute-start-line ; basic wad protocol
   #:height
   #:end-line
   #:start-column
   #:end-column
   #:items

   #:errors
   #:indentation ; wad protocol

   #:wad ; class

   #:cst-wad
   #:atom-wad
   #:cons-wad

   #:labeled-object-definition-wad
   #:labeled-object-reference-wad

   #:non-cst-wad
   #:skipped-wad
   #:comment-wad
   #:block-comment-wad

   #:semicolon-comment-wad
   #:semicolon-count ; reader

   #:ignored-wad
   #:sharpsign-wad
   #:expression ; reader
   #:sharpsign-plus-wad
   #:sharpsign-minus-wad
   #:read-suppress-wad
   #:reader-macro-wad

   #:word-wad
   #:misspelled ; reader

   #:error-wad
   #:condition) ; reader

  ;; Associated buffer protocol
  (:export
   :buffer)

  ;; Cache protocol
  ;; Extends associated buffer protocol
  (:export
   #:time-stamp

   #:line-count ; readers

   #:line-length
   #:line-contents
   #:total-width

   #:map-wads-and-spaces ; query functions
   #:find-wad-beginning-line
   #:find-wads-containing-position)

  ;; Analyzer protocol
  ;; Extends associated buffer protocol
  (:export
   #:analyzer ; class
   #:cache ; readers

   #:update))

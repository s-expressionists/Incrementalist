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
   #:children ; children protocol

   #:parent ; family relations protocol
   #:left-sibling
   #:right-sibling

   #:absolute-start-line-number ; basic wad protocol
   #:height
   #:end-line
   #:start-column
   #:end-column
   #:items

   #:indentation ; wad protocol

   #:wad ; class

   #:expression-wad

   #:labeled-object-definition-wad
   #:labeled-object-reference-wad

   #:no-expression-wad
   #:skipped-wad
   #:comment-wad
   #:block-comment-wad

   #:semicolon-comment-wad
   #:semicolon-count ; reader

   #:word-wad
   #:misspelled ; reader

   #:ignored-wad
   #:sharpsign-wad
   #:expression ; reader
   #:sharpsign-plus-wad
   #:sharpsign-minus-wad
   #:read-suppress-wad
   #:reader-macro-wad

   #:error-wad
   #:condition) ; reader

  ;; Cache protocol
  (:export
   #:line-count ; readers

   #:line-length
   #:line-contents
   #:total-width

   #:map-wads-and-spaces ; query functions
   #:find-wad-beginning-line
   #:find-wads-containing-position)

  ;; Analyzer protocol
  (:export
   #:analyzer ; class
   #:cache ; readers
   #:buffer

   #:update-cache))

(cl:defpackage #:incrementalist
  (:use
   #:common-lisp)

  (:local-nicknames
   (#:stream    #:trivial-gray-streams)

   (#:b         #:text.editor-buffer)

   (#:reader    #:eclector.reader)
   (#:readtable #:eclector.readtable)

   (#:flx       #:flexichain)

   (#:dep       #:incrementalist.dependencies)

   (#:dbg       #:incrementalist.debug))

  (:shadow
   #:condition ; reader of `error-wad'
   #:package-name)

  ;; Tokens
  (:export
   #:token ; class

   #:symbol-token ; class
   #:package-marker-1 ; readers
   #:package-marker-2
   #:package-name
   #:name

   #:uninterned-symbol-token ; classes
   #:interned-symbol-token
   #:non-existing-package-symbol-token
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
   #:state-aspect-cell
   #:state-value

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
   #:conditional-wad
   #:feature-expression ; reader
   #:skipped-conditional-wad
   #:skipped-positive-conditional-wad
   #:skipped-negative-conditional-wad
   #:read-conditional-wad
   #:read-positive-conditional-wad
   #:read-negative-conditional-wad
   #:read-suppress-wad
   #:reader-macro-wad

   #:text-wad
   #:punctuation-wad
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
   #:find-wads-containing-position
   #:map-wads-containing-position)

  ;; Analyzer protocol
  ;; Extends associated buffer protocol
  (:export
   #:make-initial-reader-state
   #:analyzer ; class
   #:cache ; readers

   #:update))

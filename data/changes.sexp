(:changes
 (:release "0.3" nil
  (:item
   (:paragraph
    "Incrementalist" "now" "uses" "the" "text.editor-buffer" "library"
    "instead" "of" "the" "Cluffer" "library" "as" "its" "implementation" "of"
    "line-oriented" "buffers" "." "The" "new" "library" "has" "slightly"
    "improved" "protocols" "and" "much" "improved" "performance" "compared"
    "to" "Cluffer" "." "The" "protocol" "improvements" "make" "the" "new"
    "library" "mostly" "but" "not" "fully" "compatible" "with" "Cluffer" ".")
   (:paragraph
    "Clients" "of" "Incrementalist" "can" "benefit" "from" "improved" "buffer"
    "and" "line" "creation" "functions" "which" "text.editor-buffer" "provides"
    (:when "manual"
     "(" (:ref :section "Introduction") "," "the" "first" "listing" "for" "an"
     "example"  "of" "the" "new" "function"
     (:tt "text.editor-buffer:make-buffer") ")")
    "." )))

 (:release "0.2" "2026-02-06"
  (:item
   (:paragraph
    "The" "biggest" "change" "is" "that" "Incrementalist" "now" "tracks"
    "dependencies" "between" "WADs" "that" "arise" "due" "to" "changes" "of"
    "the" "reader" "state" ".")
   (:paragraph
    "For" "example" "," "a" (:tt "`") "WAD" "causes" (:tt ",") "WADs" "to" "be"
    "valid" "in" "the" "following" "sub-expression" "." "However" "," "when"
    "the" (:tt "`") "WAD" "is" "changed" "to" "," "say" "," (:tt "'") "the"
    (:tt ",") "WADs" "have" "to" "be" "re-read" "with" "the" "changed" "reader"
    "state" "to" "\"witness\"" "that" "they" "are" "no" "longer" "valid" "."
    "A" "similar" "situation" "arises" "for" "sub-expressions" "which" "follow"
    "the" "consing" "dot" ".")
   (:paragraph
    "A" "different" "kind" "of" "dependency" "that" "is" "less" "localized"
    "arises" "from" "non-lexical" "changes" "to" "the" "reader" "state" "such"
    "as" "changing" "the" "current" "package" "via" (:tt "in-package") "or"
    "changing" "the" "read" "base" "via" (:tt "#.(setf *read-base* 16)") "."
    "These" "dependencies" "are" "now" "also" "tracked" "by" "Incrementalist"
    "although" "the" "mechanism" "for" "detecting" "them" "in" "expressions"
    "that" "are" "evaluated" "at" "read-time" "is" "just" "a"
    "proof-of-concept" "for" "now" "."))
  (:item
   (:paragraph
    "Initial" "unit" "tests" "have" "been" "added" "and" "subsequently"
    "extended" "to" "cover" "many" "specific" "cases" "and" "regressions" "."
    "One" "test" "reads" "the" "whole" "source" "code" "of" "Incrementalist"
    "in" "an" "incremental" "way" "and" "ensures" "that" "various" "invariants"
    "hold" "at" "all" "times" "."))
  (:item
   (:paragraph
    "Many" "parts" "of" "the" "system" "have" "been" "optimized:")
   (:paragraph
    "The" "code" "surrounding" "the" (:symbol "buffer-stream") "class" "has"
    "been" "rewritten" "to" "be" "more" "efficient" ".")
   (:paragraph
    "Construction" "and" "linking" "of" "WADs" "has" "been" "optimized" "."))
  (:item
   (:paragraph
    "The" "class" "hierarchies" "rooted" "at" (:symbol "token") "and"
    (:symbol "wad") "have" "been" "revised" "multiple" "times" "to" "better"
    "represent" "the" "kinds" "and" "properties" "of" "the" "relevant"
    "concrete" "syntax" "objects" "." "Examples" "are" "tokens" "for" "numbers"
    "and" "symbols" "as" "well" "as" "WADs" "for" "labeled" "objects" "and"
    "read-time" "conditional" "expressions" ".")
   (:paragraph
    "One" "particular" "addition" "are" (:symbol "word-wad") "and"
    (:symbol "punctuation-wad") "which" "are" "used" "to" "represent" "the"
    "components" "of" "the" "strings" "in" "symbol" "names" "and" "comments"
    "in" "detail" "."))
  (:item
   (:paragraph
    "Under" "certain" "conditions" "," (:symbol "word-wad") "instances" "are"
    "marked" "as" "being" "spelled" "correctly" "or" "incorrectly" "using"
    "the" "spell" "library" "." "As" "a" "consequence" "," "Incrementalist"
    "now" "depends" "on" "spell" "."))
  (:item
   (:paragraph
    "The" "concept" "of" "some" "WADs" "having" "a" "dual" "nature" "in" "the"
    "sense" "of" "conforming" "to" "the" "WAD" "protocol" "as" "well" "as"
    "the" "CST" "protocol" "has" "been" "introduced" "." "This" "concept"
    "will" "be" "important" "when" "WADs" "will" "be" "passed" "to" "the"
    "s-expression-syntax" "library" "in" "order" "to" "be" "parsed" "as"
    "s-expressions" "."))
  (:item
   (:paragraph
    "Assertions" "which" "check" "fundamental" "invariants" "of"
    "Incrementalist's" "data-structures" "as" "well" "as" "debugging" "tools"
    "are" "now" "available" "in" "separate" "files" "which" "can" "be"
    "loaded" "when" "needed" "."))
  (:item
   (:paragraph
    "Some" "of" "the" "functions" "which" "query" "the" "cache" "based" "on"
    "a" "given" "buffer" "location" "now" "accept" "additional" "arguments"
    "which" "control" "whether" "the" "bounds" "of" "WADs" "should" "match"
    "the" "given" "location" "inclusively" "or" "exclusively" "."))
  (:item
   (:paragraph
    "The" "manual" "has" "been" "extensively" "but" "not" "yet" "fully"
    "rewritten" "and" "extended" ".")))

 (:release "0.1" "not released" ; Commit 832dfea6
  (:item
   (:paragraph
    "Initial" "version" "extracted" "from" "Second" "Climacs" "repository"
    "with" "core" "architecture" ":" "analyzer" "," "cache" "," "WADs" "and"
    "parsing" "via" "Eclector" "." "This" "version" "includes" "a"
    "Texinfo-based" "manual" "which" "has" "been" "extracted" "and"
    "converted" "from" "the" "LaTeX-based" "manual" "of" "Second" "Climacs"
    "."))))

SBCL ?= sbcl
QUICKLISP_DIRECTORY ?= $${HOME}/quicklisp/
ASDF_SOURCE_REGISTRY ?= (quote (:source-registry (:directory "'$$(pwd)/..'") :ignore-inherited-configuration))

NAME = incrementalist

SOURCES = $(wildcard *.texi)

GENERATED = generated-version.texi               \
            generated-token-class-hierarchy.texi \
            generated-wad-class-hierarchy.texi

INPUTS = $(SOURCES) $(GENERATED)

FIGURES = $(wildcard *.svg)
PDFS    = $(subst .svg,.pdf,$(FIGURES))
IMAGES  = $(subst .svg,.png,$(FIGURES))

.PHONY: all clean

all : $(NAME).info $(NAME).pdf $(NAME).html

%.pdf: %.svg
	rsvg-convert --format=pdf1.5 $< > $@

%.png: %.svg
	rsvg-convert --format=png $< > $@

generated-version.texi: ../data/version-string.sexp
	$(SBCL) --noinform --disable-debugger --no-sysinit --no-userinit                     \
          --eval '(require :asdf)'                                                           \
          --eval '(format t "@macro sysversion~%~A~%@end macro" (uiop:read-file-form "$<"))' \
          --quit                                                                             \
          > $@

generated-token-class-hierarchy.texi generated-wad-class-hierarchy.texi &: \
  ../tools/generate-documentation.lisp \
  ../code/token.lisp                   \
  ../code/wad.lisp
	$(SBCL) --noinform --disable-debugger --no-sysinit --no-userinit                   \
          --load "$(QUICKLISP_DIRECTORY)/setup.lisp"                                       \
          --eval '(asdf:initialize-source-registry $(ASDF_SOURCE_REGISTRY))'               \
          --eval '(ql:quickload (list "incrementalist" "utilities.print-tree") :silent t)' \
          --load '../tools/generate-documentation.lisp'                                    \
          --quit --end-toplevel-options                                                    \
          generated-token-class-hierarchy.texi generated-wad-class-hierarchy.texi

$(NAME).info: $(INPUTS) $(IMAGES)
	makeinfo --output $@ $(NAME).texi

$(NAME).pdf: $(INPUTS) $(PDFS)
	texi2pdf $(NAME).texi

$(NAME).html: $(INPUTS) $(IMAGES) $(wildcard *.css)
	makeinfo --html --no-split --no-headers                                 \
                 --css-include=style-common.css --css-include=style-single.css  \
                 $(NAME).texi

clean:
	rm -f *.cp *.cps *.fn *.fns                     \
              *.aux *.toc *.log                         \
              *.pdf *.png                               \
              $(NAME).info $(NAME).pdf $(NAME).html     \
              $(GENERATED)

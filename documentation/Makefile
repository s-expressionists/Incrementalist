NAME = incrementalist

SOURCES = $(wildcard *.texi)

GENERATED = generated-version.texi

INPUTS = $(SOURCES) $(GENERATED)

FIGURES = $(wildcard *.svg)
PDFS    = $(subst .svg,.pdf,$(FIGURES))
IMAGES  = $(subst .svg,.png,$(FIGURES))

.PHONY: all clean

all : $(NAME).info $(NAME).pdf $(NAME).html

%.pdf: %.svg
	rsvg-convert --format=pdf $< > $@

%.png: %.svg
	rsvg-convert --format=png $< > $@

generated-version.texi: ../data/version-string.sexp
	sbcl --noinform --disable-debugger --no-sysinit --no-userinit                           \
	     --eval '(require :asdf)'                                                           \
	     --eval '(format t "@macro sysversion~%~A~%@end macro" (uiop:read-file-form "$<"))' \
	     --quit                                                                             \
	  > $@

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

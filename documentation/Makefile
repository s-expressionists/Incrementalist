NAME = incrementalist

SOURCES = $(wildcard *.texi)

GENERATED = generated-version.texi

INPUTS = $(SOURCES) $(GENERATED)

FIGS    = $(wildcard *.fig)
PDFS    = $(subst .fig,.pdf,$(FIGS))
FIGURES = $(PDFS)
IMAGES  = $(subst .pdf,.png,$(FIGURES))

.PHONY: all clean

all : $(NAME).info $(NAME).pdf $(NAME).html

%.pdf: %.fig
	fig2dev -Lpdftex -m 0.75  $< $@

%.png: %.fig
	fig2dev -Lpng -m 1.2 $< $@

generated-version.texi: ../data/version-string.sexp
	sbcl --noinform --disable-debugger --no-sysinit --no-userinit                           \
	     --eval '(require :asdf)'                                                           \
	     --eval '(format t "@macro sysversion~%~A~%@end macro" (uiop:read-file-form "$<"))' \
	     --quit                                                                             \
	  > $@

$(NAME).info: $(INPUTS) $(IMAGES)
	makeinfo --output $@ $(NAME).texi

$(NAME).pdf: $(INPUTS) $(FIGURES)
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

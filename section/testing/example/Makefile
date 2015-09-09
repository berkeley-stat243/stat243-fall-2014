BUILDDIR  = _build
BINDIR    = ../../bin
TEX2PDF  := cd $(BUILDDIR) && TEXINPUTS="..:" pdflatex -shell-escape
KNIT     := cd $(BUILDDIR) && $(BINDIR)/knitr

all: clean example

clean:
	rm -rf $(BUILDDIR)/* *.pdf

test:
	R --quiet -e 'library(testthat); test_file("test.r")'

$(BUILDDIR):
	mkdir -p $@

$(BUILDDIR)/%.pdf: %.tex
	($(TEX2PDF) $(<F) 1>/dev/null)


%: %.tex $(BUILDDIR)
	($(KNIT) ../$<)
	($(TEX2PDF) $(@)-out.tex)
	($(TEX2PDF) $(@)-out.tex)
	cp $(BUILDDIR)/$(@)-out.pdf $(@).pdf 

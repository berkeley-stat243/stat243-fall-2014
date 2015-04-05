BUILDDIR  = _build
RSCRIPT  := cd $(BUILDDIR) && Rscript -e
TEX2PDF  := cd $(BUILDDIR) && pdflatex -interaction=batchmode

all: clean git-II.pdf

clean:
	rm -rf $(BUILDDIR)/* *.pdf *.png

$(BUILDDIR):
	mkdir -p $(@)

git-II.pdf: git-II.tex $(BUILDDIR)
	$(RSCRIPT) "library(knitr); knit('../$(<F)', '$(<)')"
	($(TEX2PDF) $(<))
	($(TEX2PDF) $(<))
	cp $(BUILDDIR)/$(@) .

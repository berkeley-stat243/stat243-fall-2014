BUILDDIR  = _build
RSCRIPT := cd $(BUILDDIR) && Rscript -e
TEX2PDF  := cd $(BUILDDIR) && pdflatex -interaction=batchmode

all: clean ps4-review

clean:
	rm -rf $(BUILDDIR)/* *.pdf

$(BUILDDIR):
	mkdir -p $@

#$(BUILDDIR)/%.pdf: $(BUILDDIR)/%.tex
#	($(TEX2PDF) $(<F) 1>/dev/null)


ps4-review: ps4-review.tex $(BUILDDIR)
	$(RSCRIPT) "library(knitr); knit('../$(<F)', '$(@).tex')"
	($(TEX2PDF) $(@))
	($(TEX2PDF) $(@))
	cp $(BUILDDIR)/$(@).pdf $(@).pdf 

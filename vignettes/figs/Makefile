%.png:
	latexmk -pdf $*
	magick convert -density 600 -background white -flatten *.pdf $*.png
	latexmk -C
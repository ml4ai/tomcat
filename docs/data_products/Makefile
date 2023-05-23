all: data_products.pdf

data_products.pdf : build/data_products.pdf
	cp -f build/data_products.pdf data_products.pdf

build/data_products.pdf : data_products.tex
	./scripts/do_latex data_products.tex build/

/Volumes/home/kobus/ivilab/texinputs/IVILAB-notebook-preamble.tex : $(HOME)/ivilab/texinputs/IVILAB-book-preamble.tex
	touch /Volumes/home/kobus/ivilab/texinputs/IVILAB-notebook-preamble.tex

/Volumes/home/kobus/ivilab/texinputs/IVILAB-book-preamble.tex : $(HOME)/ivilab/texinputs/IVILAB-base-preamble.tex
	touch /Volumes/home/kobus/ivilab/texinputs/IVILAB-book-preamble.tex

.PHONY clean :
	rm -f  data_products.pdf build/data_products.pdf build/data_products.aux build/data_products.out build/data_products.log

# For live previewing on macOS with the Skim PDF reader
watch:
	latexmk -pdflatex -pvc -e '$$pdf_previewer=q[open -a Skim]' -outdir=build data_products.tex

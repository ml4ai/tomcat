# This Makefile is build by the script build_latex_makefile. 
# Manual changes will be overwitten. 
 
.PHONY : all 
all : data_products.pdf
 
data_products.pdf : build/data_products.pdf
	cp -f build/data_products.pdf data_products.pdf
 
build/data_products.pdf : data_products.tex
	./scripts/do_latex data_products.tex build/
 
data_products.tex : sections/overview.tex sections/raw_data.tex sections/derived_data.tex
	touch data_products.tex

.PHONY : clean
clean : 
	rm -f  data_products.pdf build/data_products.pdf build/data_products.aux build/data_products.out build/data_products.log

.PHONY : watch
watch : 
	latexmk -pdflatex -pvc -e '$$pdf_previewer=q[open -a Skim]' -outdir=build/ data_products.tex

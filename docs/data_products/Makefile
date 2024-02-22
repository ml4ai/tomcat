all:
	latexmk -pdf -outdir=build data_products.tex

# For live previewing on macOS with the Skim PDF reader
watch:
	latexmk -pdf -pvc -e '$$pdf_previewer=q[open -a Skim]' -outdir=build data_products.tex

clean:
	latexmk -c data_products.tex

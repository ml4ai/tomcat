all:
	latexmk -lualatex -g -interaction=nonstopmode -outdir=build main.tex

# For live previewing on macOS with the Skim PDF reader
watch:
	latexmk -lualatex -pvc -e '$$pdf_previewer=q[open -a Skim]' -outdir=build main.tex

clean:
	latexmk -C *.tex

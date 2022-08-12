all:
	latexmk -lualatex -shell-escape -outdir=build -auxdir=build main.tex

# For live previewing on macOS with the Skim PDF reader
watch:
	latexmk -lualatex -shell-escape -pvc -e '$$pdf_previewer=q[open -a Skim]' -outdir=build -auxdir=build main.tex

clean:
	latexmk -c main.tex

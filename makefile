# Makefile
# Only usable on Unix-based systems or Windows Linux Subsystem installed.

TARGETS = report.tex

default:
	pdflatex -shell-escape $(TARGETS)

rebuild: clean default

clean:
	find . -type f -name '*.aux'  -delete
	find . -type f -name '*.dvi'  -delete
	find . -type f -name '*.log'  -delete
	find . -type f -name '*.toc'  -delete
	rm -rf *.pdf
	find . -type f -name '*.fdb_latexmk'  -delete
	find . -type f -name '*.fls'  -delete
	find . -type f -name '*.pyg'  -delete
	find . -type f -name '*.out'  -delete
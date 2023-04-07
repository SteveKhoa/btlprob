# Makefile
# Only usable on Unix-based systems or Windows Linux Subsystem installed.

TARGETS = report.tex

default:
	pdflatex -shell-escape $(TARGETS) | grep "Warning"

rebuild: clean default

clean:
	find . -type f -name '*.aux'  -delete
	find . -type f -name '*.dvi'  -delete
	find . -type f -name '*.log'  -delete
	find . -type f -name '*.toc'  -delete
	find . -type f -name '*.pdf'  -delete
	find . -type f -name '*.fdb_latexmk'  -delete
	find . -type f -name '*.fls'  -delete
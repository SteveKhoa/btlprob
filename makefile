# Makefile
# Only usable on Unix-based systems or Windows Linux Subsystem installed.

TARGETS = report.tex

default:
	pdflatex -shell-escape $(TARGETS) | grep "Warning"

clean:
	find . -type f -name *'.aux' '*.dvi' '*.log' '*.to'c '*.pdf' -delete
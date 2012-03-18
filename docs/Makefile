FileName = bayesianModelDefinition

all:
	latex $(FileName)
	bibtex $(FileName)
	latex $(FileName)
	latex $(FileName)
	dvipdf $(FileName).dvi

clean:
	rm -f *.dvi *.pdf *.log *.aux *.toc *.zip *.lox *.bbl *.blg

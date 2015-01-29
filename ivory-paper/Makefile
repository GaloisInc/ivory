.PHONY: clean veryclean

WORK_DIR=pdflatex
TGT=ivory
BIB=paper
#latex=TEXINPUTS=".:./style:" pdflatex -output-directory $(WORK_DIR)
latex=pdflatex -output-directory $(WORK_DIR)
tex=latex -output-directory $(WORK_DIR)

all: $(TGT).pdf

$(TGT).pdf: $(WORK_DIR) *.tex
	$(latex) $(TGT)
	bibtex $(WORK_DIR)/$(TGT).aux
	$(latex) $(TGT)
	bibtex $(WORK_DIR)/$(TGT).aux
	# echo "\balance" >> $(WORK_DIR)/$(TGT).bbl
	$(latex) $(TGT)
	cp $(WORK_DIR)/$(TGT).pdf .
	# cp $(WORK_DIR)/$(TGT).pdf icfp35ep-hickey.pdf
	# pdf2ps icfp35ep-hickey.pdf
	# -rm icfp35ep-hickey.zip
	# zip -r icfp35ep-hickey.zip *.tex *.cls *.bib figures Makefile

$(TGT).dvi: $(WORK_DIR) $(TGT).tex
	$(tex) $(TGT)
	bibtex $(WORK_DIR)/$(TGT).aux
	$(tex) $(TGT)
	$(tex) $(TGT)

$(WORK_DIR):
	mkdir -p $(WORK_DIR)

clean:
	-rm -rf $(WORK_DIR)

veryclean: clean
	-rm $(TGT).pdf

NAME = p
PAPER = $(NAME).ps
TEXTAR = $(NAME).tex

# put eps figures here to get dependency tracking working
FIGURES =

# get all the tex files
BASE_TEX = $(wildcard *.tex)

# put tex files here to get dependency tracking working
TEXSRC = $(BASE_TEX)

all: $(PAPER)

LATEX := latex

$(NAME).pdf: $(NAME).ps
	ps2pdf $(NAME).ps $(NAME).pdf
$(PAPER): paper.dvi
	dvips -Ppdf -t letter -G0 -o $(NAME).ps paper.dvi

# $(LATEX) paper && bibtex paper && $(LATEX) paper && $(LATEX) paper
paper.dvi: $(TEXSRC) $(FIGURES) ../../biblio.bib
	$(LATEX) paper && bibtex paper && $(LATEX) paper && $(LATEX) paper

.PHONY: clean
clean:
	-rm -f *.dvi *.ps *.pdf $(TEXTAR)
	-rm -f *.bbl *.blg *.aux *.log
	-rm -f *.bak *~

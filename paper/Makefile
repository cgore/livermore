NAME=paper

all: $(NAME).dvi $(NAME).ps $(NAME).pdf

$(NAME).aux: $(NAME).latex
	latex $(NAME).latex

$(NAME).dvi: $(NAME).latex $(NAME).bib $(NAME).aux
	bibtex $(NAME)
	latex $(NAME).latex
	latex $(NAME).latex

$(NAME).ps: $(NAME).dvi
	dvips -o $(NAME).ps $(NAME).dvi

$(NAME).pdf: $(NAME).ps
	ps2pdf $(NAME).ps

print: $(NAME).ps
	lpr $(NAME).ps

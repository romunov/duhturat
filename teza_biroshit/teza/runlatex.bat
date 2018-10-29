# cleanup if something went wrong in the previous run
rm *.aux
rm *.bbl
rm *.log
rm *.lof
rm *.toc
rm *.blg
rm *.lot
rm *.out

pdflatex 0_lustrik_doktorat
bibtex 0_lustrik_doktorat
pdflatex 0_lustrik_doktorat
pdflatex 0_lustrik_doktorat
pdflatex 0_lustrik_doktorat

mv 0_lustrik_doktorat.pdf lustrik_doktorat.pdf

rm *.aux
rm *.bbl
rm *.log
rm *.lof
rm *.toc
rm *.blg
rm *.lot
rm *.out
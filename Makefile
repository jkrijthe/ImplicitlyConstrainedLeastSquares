short: tabfig-short document-short
long: tabfig-short document-long

experiments-short:
	Rscript code/ExperimentBenchmarkLeastSquares.R
	Rscript code/ExperimentErrorCurvesLeastSquares.R

tabfig-short:
	R CMD BATCH code/generate-table-crossvalidation-small.R
	R CMD BATCH code/generate-figure-learningcurve-small.R

document-short:
	cp figures/figure2.tex icls-short/figure2.tex
	cd icls-short; \
	pdflatex icls-short.tex; \
	bibtex icls-short; \
	pdflatex icls-short.tex; \
	pdflatex icls-short.tex

experiments-long:
	Rscript code/ExperimentBenchmarkLeastSquares.R
	Rscript code/ExperimentErrorCurvesLeastSquares.R

tabfig-long:
	R CMD BATCH code/VisualizeBenchmarkLeastSquaresSSL.R
	R CMD BATCH code/VisualizeErrorCurvesLeastSquaresSSL.R
	R CMD BATCH code/constrainedregion.R
	pdfcrop figures/constrainedregion.pdf
	pdfcrop figures/Learningcurves1.pdf
	pdfcrop figures/Learningcurves2.pdf
	
document-long:
	cd icls-long; \
	pdflatex icls-long.tex; \
	bibtex icls-long; \
	pdflatex icls-long.tex; \
	pdflatex icls-long.tex
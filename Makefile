all: experiments tabfig document

experiments:
	R ExperimentBenchmarkLeastSquares.R
	R ExperimentErrorCurvesLeastSquares.R

tabfig:
	R CMD BATCH VisualizeBenchmarkLeastSquaresSSL.R
	R CMD BATCH VisualizeErrorCurvesLeastSquaresSSL.R
	R CMD BATCH constrainedregion.R
	pdfcrop constrainedregion.pdf
	pdfcrop Learningcurves1.pdf
	pdfcrop Learningcurves2.pdf

document:
	latex paper.tex
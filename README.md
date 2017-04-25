# Implicitly Constrained Least Squares Classification

This repository contains the code to run the experiments and generate the conference paper "Implicitly Constrained Least Squares Classification" and the extended version of this work "Robust Semi-supervised Least Squares Classification by Implicit Constraints" by Jesse H. Krijthe and Marco Loog:

Krijthe, J. H., & Loog, M. (2017). Robust Semi-supervised Least Squares Classification by Implicit Constraints. Pattern Recognition, 63, 115–126. https://doi.org/10.1016/j.patcog.2016.09.009

Krijthe, J. H., & Loog, M. (2015). Implicitly Constrained Semi-Supervised Least Squares Classification. In E. Fromont, T. De Bie, & M. van Leeuwen (Eds.), Advances in Intelligent Data Analysis XIV. Lecture Notes in Computer Science, vol 9385. (pp. 158–169). Saint Étienne. France: Springer. https://doi.org/10.1007/978-3-319-24465-5_14


The actual code for the methods involved has been placed in a separate R package (RSSL). Version 8814741a2870d4a1b06eef1fdf97e1342410eec3 of this package contains the code used to run the experiments in the paper.

## Requirements
make
latex
pdfcrop
R (3.0.0) with packages
	RSSL (commit: 8814741a2870d4a1b06eef1fdf97e1342410eec3), install using install_github("jkrijthe/RSSL",ref="8814741a2870d4a1b06eef1fdf97e1342410eec3") from the devtools package
	createdatasets
	ggplot2

The experiments were originally run under Mac OS X 10.10.

## Acknowledgement
This publication was supported by the Dutch national program COMMIT, project P23.
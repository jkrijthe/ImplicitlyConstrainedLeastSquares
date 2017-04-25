This repository contains the code to run the experiments and generate the conference paper "Implicitly Constrained Least Squares Classification" and the extended version of this work "Robust Semi-supervised Least Squares Classification by Implicit Constraints" by Jesse H. Krijthe and Marco Loog. The actual code for the methods involved has been placed in a separate R package (RSSL). Version 8814741a2870d4a1b06eef1fdf97e1342410eec3 of this package contains the code used to run the experiments in the paper.

#Requirements
make
latex
pdfcrop
R (3.0.0) with packages
	RSSL (commit: 8814741a2870d4a1b06eef1fdf97e1342410eec3), install using install_github("jkrijthe/RSSL",ref="8814741a2870d4a1b06eef1fdf97e1342410eec3") from the devtools package
	createdatasets
	ggplot2

The experiments were originally run under Mac OS X 10.10.

#Reproducing the paper
To run the experiments, generate the tables and figures and generate the PDF document from the Latex sources, run the following command:

```
make all
```

To use the experimental results used to generate the paper for the figures and tables, run:

```
make
```

#Acknowledgement
This publication was supported by the Dutch national program COMMIT, project P23.
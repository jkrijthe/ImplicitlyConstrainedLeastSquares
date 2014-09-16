Code accompanying the paper "Implicitly Constrained Least Squares Classification" by Jesse H. Krijthe and Marco Loog. The actual code for the methods involved has been placed in a separate R package (RSSL). Version 0.1 of this package contains the code used to run the experiments in the paper.

#Requirements
make
latex
pdfcrop
R (3.0.0) with packages
	RSSL (0.1), install using install_github("RSSL",username="jkrijthe",ref="0.1") from the devtools package 
	ggplot2

The experiments were originally run under Mac OS X 10.8, but should work in different setups as well.

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
This publication was supported by the Dutch national program COMMIT.
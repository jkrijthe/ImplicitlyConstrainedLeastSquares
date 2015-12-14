---
output: pdf_document
---
### Title
Robust Semi-supervised Least Squares Classification by Implicit Constraints

### Authors

\begin{description}
  \item[] Jesse H. Krijthe
  \item[] Pattern Recognition Laboratory,
  \item[] Delft University of Technology,
  \item[] Mekelweg 4, 2628CD Delft, The Netherlands
  \item[] jkrijthe@gmail.com
\end{description}


\begin{description}
  \item[] Marco Loog
 \item[]Pattern Recognition Laboratory, 
 \item[]Delft University of Technology, 
 \item[]Mekelweg 4, 2628CD Delft, The Netherlands
 \item[]m.loog@tudelft.nl
\end{description}

### Corresponding author
Jesse H. Krijthe

### Abstract
We introduce the implicitly constrained least squares (ICLS) classifier, a novel semi-supervised version of the least squares classifier. This classifier minimizes the squared loss on the labeled data among the set of parameters implied by all possible labelings of the unlabeled data. Unlike other discriminative semi-supervised methods, this approach does not introduce explicit additional assumptions into the objective function, but leverages implicit assumptions already present in the choice of the supervised least squares classifier. This method can be formulated as a quadratic programming problem and its solution can be found using a simple gradient descent procedure. We prove that, in a limited 1-dimensional setting, this approach never leads to performance worse than the supervised classifier.
Experimental results show that also in the general multidimensional case performance improvements can be expected, both in terms of the squared loss that is intrinsic to the classifier, as well as in terms of the expected classification error.

### Keywords
Semi-supervised learning, Robust, Least squares classification

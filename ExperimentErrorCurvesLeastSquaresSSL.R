## ExperimentErrorCurvesLeastSquaresSSL
rm(list=ls())
library(devtools)
load_all("~/Dropbox/Code/RSSL")


## Load data
load("~/Data/Datasets.RData")
datasets<-datasets[c(1:16)]
repeats<-100
n_labeled<-"enough"
description<-"PCArank"

## Settings
verbose <- TRUE
classifiers<-c(function(X,y,X_u,y_u) {SelfLearning(X, y, X_u, method=LeastSquaresClassifier,intercept=TRUE,x_center=TRUE)},
               function(X,y,X_u,y_u) {LeastSquaresClassifier(X,y,intercept=TRUE,x_center=TRUE) },
               function(X,y,X_u,y_u) {LeastSquaresClassifier(rbind(X,X_u),c(y,y_u),intercept=TRUE,x_center=TRUE) },
               function(X,y,X_u,y_u) {UCLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE) },
               function(X,y,X_u,y_u) {ICLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE) }
               )
preprocessing<-c(function(X,y,X_u,y_u) {})



## Calculate Learning Curves
errorcurves<-lapply(names(datasets),function(dname){
							cat(dname,"\n");
							classname<-all.vars(modelforms[[dname]])[1]
							current_data<-SSLDataFrameToMatrices(modelforms[[dname]],datasets[[dname]],intercept=FALSE)
                                   if (n_labeled=="enough") { n_l<-max(ncol(current_data$X)+5,20) }
                                   else {n_l<-n_labeled}
							ErrorCurveSSL2(current_data$X,current_data$y,classifiers,n_l=n_l,s=2^(1:12),repeats=repeats,verbose=verbose)})

## Calculate Transductive Cross Validation Benchmarks
# cvresults<-lapply(names(datasets)[1:8],function(dname){
# 							cat(dname,"\n");
# 							classname<-all.vars(modelforms[[dname]])[1]
# 							current_data<-SSLDataFrameToMatrices(modelforms[[dname]],datasets[[dname]],intercept=FALSE)
                                   # if (n_labeled=="enough") { n_l<-max(ncol(current_data$X)+5,20) }
                                   # else {n_l<-n_labeled}
# 							CrossValidationTransductive(current_data$X,current_data$y,classifiers,n_l=n_l,s=2^(0:10),repeats=1000,verbose=T)})

## Save results
save.image("~/Dropbox/Results/LearningCurves-justenoughplus5or20-100repeats-style1.RData")

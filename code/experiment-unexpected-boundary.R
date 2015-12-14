library(methods)
library(RSSL)
library(createdatasets)
library(randomForest)
library(parallel)

start_time <- Sys.time()
setdatadir("~/Data")
datasets<-list("Assumption Correct"=generate2ClassGaussian(d=2,expected=TRUE),
               "Assumption Incorrect"=generate2ClassGaussian(d=2,expected=FALSE))

models <- list("Assumption Correct"=formula(Class~.),
               "Assumption Incorrect"=formula(Class~.)
               )

## Settings
classifiers<-list(
  "Supervised"=function(X,y,X_u,y_u) {LeastSquaresClassifier(X,y,intercept=TRUE,x_center=TRUE,scale=FALSE) },
  "Self-Learning"=function(X,y,X_u,y_u) {SelfLearning(X, y, X_u, method=LeastSquaresClassifier,intercept=TRUE,x_center=TRUE,scale=FALSE)},
  "USM"=function(X,y,X_u,y_u) {USMLeastSquaresClassifier(X,y,X_u,intercept=FALSE,x_center=TRUE,y_scale=TRUE) }, 
  "ICLS"=function(X,y,X_u,y_u) {ICLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE,scale=FALSE) },
  "EMLeastSquaresClassifier"=function(X,y,X_u,y_u) {EMLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE,scale=FALSE,eps = 1e-4) }, 
  "Oracle"=function(X,y,X_u,y_u) {LeastSquaresClassifier(rbind(X,X_u),unlist(list(y,y_u)),intercept=TRUE,x_center=TRUE,scale=FALSE) }
)

modelforms <- models
datasets <- datasets
repeats <- 100
n_labeled <- 4
pca <- FALSE
description <-"scale"
verbose <- TRUE

## Calculate Learning Curves
errorcurves<-lapply(names(datasets),function(dname){
  cat(dname,"\n");
  data <- data.frame(datasets[[dname]]) 
  classname<-all.vars(modelforms[[dname]])[1]
  #current_data<-SSLDataFrameToMatrices(modelforms[[dname]],datasets[[dname]],intercept=FALSE)
  
  X <- model.matrix(modelforms[[dname]],datasets[[dname]])
  X <- X[,colnames(X)!="(Intercept)"]
  X <- X[,apply(X, 2, var, na.rm=TRUE) != 0] # Remove constant columns
  X <- scale(X) # Pre-scale data
  
  if (pca) {
    t_pca <- princomp(X)
    n_comp <- sum(cumsum(t_pca$sdev^2)/sum(t_pca$sdev^2)<0.99)
    n_comp <- n_comp #min(c(n_comp,floor(n_labeled/2)))
    X <- t_pca$scores[,1:n_comp]
  }
  y <- data[,classname]
  
  if (n_labeled=="enough") { n_l <- max(ncol(X)+5,20) }
  else if (n_labeled=="d") { n_l <- ncol(X)+1 }
  else if (n_labeled=="2d") { n_l <- ncol(X)*2 }
  else {n_l<-n_labeled}
  
  ErrorCurveSSL(X,y,classifiers,n_l=n_l,s=2^(0:8),repeats=repeats, verbose=verbose, with_replacement = FALSE, n_test = 1000)
})

names(errorcurves) <- names(datasets)

total_time <- Sys.time() - start_time 
print(total_time)
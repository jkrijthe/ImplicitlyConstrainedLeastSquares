library(methods)
library(RSSL)
library(createdatasets)
library(randomForest)
library(parallel)

source("code/datasets-and-classifiers.R")

#set.seed(42)

start_time <- Sys.time()

modelforms <- models
datasets <- datasets
repeats <- 1000
n_labeled <- "enough"
pca <- FALSE
description <-"testing"
verbose <- TRUE

## Calculate Learning Curves
errorcurves <- mclapply(names(datasets),function(dname){
  cat(dname,"\n");
  data <- data.frame(datasets[[dname]]) 
  classname<-all.vars(modelforms[[dname]])[1]
  
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
  
  result <- LearningCurveSSL(X,y,classifiers,n_l=n_l,s=2^(0:10),
                   repeats=repeats, 
                   verbose=verbose, 
                   with_replacement = FALSE, 
                   n_test = 1000,
                   measures = list(Error = measure_error,
                                   "Loss Labeled" = measure_losslab,
                                   "Loss Train" = measure_losstrain,
                                   "Loss Test" = measure_losstest
                                   )
  )
  save(result,file=paste0("data/backup-",dname,"-",description,".RData"))
  result
},mc.cores=4)

names(errorcurves) <- names(datasets)

total_time <- Sys.time() - start_time 
print(total_time)

dir.create("data/",showWarnings = FALSE)
save(errorcurves,classifiers,repeats,n_labeled,total_time,file=paste0("Data/learningcurves-",repeats,"repeats-",n_labeled,"labeled-",description,".RData"))
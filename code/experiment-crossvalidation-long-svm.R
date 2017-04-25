library(methods)
library(RSSL)
library(randomForest)
library(parallel)

set.seed(42)

source("code/datasets-and-classifiers.R")
classifiers<-list(
  "Supervised"=function(X,y,X_u,y_u) { svmlin(X,y,X_u, algorithm=1,lambda=1,lambda_u=1,pos_frac=mean(as.numeric(y)==2)) },
  "Self-Learning"=function(X,y,X_u,y_u) {SelfLearning(X, y, X_u, method=svmlin,intercept=TRUE,x_center=TRUE,scale=FALSE, algorithm=1,lambda=1,lambda_u=1,pos_frac=mean(as.numeric(y)==2))},
  "TSVM"=function(X,y,X_u,y_u) { svmlin(X,y,X_u, algorithm=2,lambda=1,lambda_u=1,pos_frac=mean(as.numeric(y)==2))},
  "Oracle"=function(X,y,X_u,y_u) { svmlin(rbind(X,X_u),unlist(list(y,y_u)),X_u, algorithm=1,lambda=1,lambda_u=1,pos_frac=mean(as.numeric(y)==2)) }
)

start_time <- Sys.time()

modelforms <- models[1:16]
datasets <- datasets[1:16]
repeats <- 101
n_labeled <- "enough"
pca <- FALSE
description <-"final"
verbose <- TRUE

## Calculate Learning Curves
cvresults <- mclapply(names(datasets),function(dname){
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
  
  
  result <- CrossValidationSSL(X,y,classifiers,
                               measures = list(Error = measure_error,
                                               "Loss Labeled" = measure_losslab,
                                               "Loss Train" = measure_losstrain,
                                               "Loss Test" = measure_losstest
                               ), 
                               repeats=repeats,
                               n_labeled=n_l,
                               verbose=TRUE,
                               k=10,
                               low_level_cores = 3
                               
  )
  #save(result,file=paste0("data/cv-backup-",dname,".RData"))
  result
},mc.cores = 1)

names(cvresults) <- names(datasets)

total_time <- Sys.time() - start_time 
print(total_time)

dir.create("data/",showWarnings = FALSE)
save(cvresults,classifiers,repeats,n_labeled,total_time,file=paste0("data/crossvalidation-svm-",repeats,"repeats-",n_labeled,"labeled-",description,".RData"))
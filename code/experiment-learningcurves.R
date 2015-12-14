library(methods)
library(RSSL)
library(createdatasets)
library(randomForest)
library(parallel)

start_time <- Sys.time()
setdatadir("~/Data")
datasets<-list("Haberman"=createHaberman(),
               "Ionosphere"=createIonosphere(),
               "Parkinsons"=createParkinsons(),
               "Diabetes"=na.roughfix(createDiabetes()),
               "Sonar"=createSonar(),
               "SPECT"=createSPECT(),
               "SPECTF"=createSPECTF(),
               "Transfusion"=createTransfusion(),
               "WDBC"=createWDBC(),
               "Mammography"=na.roughfix(createMammographicMass()),
               "Digit1"=createDigit1(),
               "USPS"=createUSPS(),
               "COIL2"=createCOIL2(),
               "BCI"=createBCI(),
               "g241c"=createG241C(),
               "g241d"=createG241N())

models <- list("Haberman"=formula(Survival~.),
               "Ionosphere"=formula(Return~.),
               "Parkinsons"=formula(status~ . -subject -recording),
               "Diabetes"=formula(Diabetes~.),
               "Sonar"=formula(Label ~ .),
               "SPECT"=formula(Diagnosis ~ .),
               "SPECTF"=formula(Diagnosis ~ .),
               "Transfusion"=formula(Donated ~ .),
               "WDBC"=formula(Diagnosis ~ . -ID),
               "Mammography"=formula(Severity ~ . -BIRADS),
               "Digit1"=formula(Class ~ .),
               "USPS"=formula(Class ~ .),
               "COIL2"=formula(Class ~ .),
               "BCI"=formula(Class ~ .),
               "g241c"=formula(Class ~ .),
               "g241d"=formula(Class ~ .))

## Settings
classifiers<-list(
  "Supervised"=function(X,y,X_u,y_u) {LeastSquaresClassifier(X,y,intercept=TRUE,x_center=TRUE,scale=FALSE) },
  "Self-Learning"=function(X,y,X_u,y_u) {SelfLearning(X, y, X_u, method=LeastSquaresClassifier,intercept=TRUE,x_center=TRUE,scale=FALSE)},
  "USM"=function(X,y,X_u,y_u) {USMLeastSquaresClassifier(X,y,X_u,intercept=FALSE,x_center=TRUE,y_scale=TRUE) }, 
  "ICLS"=function(X,y,X_u,y_u) {ICLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE,scale=FALSE) },
  #"EMLS"=function(X,y,X_u,y_u) {EMLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE,scale=FALSE) }, 
  "ICLS_prior"=function(X,y,X_u,y_u) { ICLeastSquaresClassifier(X,y,X_u,x_center=FALSE,scale=FALSE, lambda_prior=1000000, trueprob=mean(model.matrix(~y-1,data.frame(y=y_u))[,1])) },
  #"Projection"=function(X,y,X_u,y_u) {ICLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE,scale=FALSE,projection="semisupervised") },
  #"Projection_prior"=function(X,y,X_u,y_u) { ICLeastSquaresClassifier(X,y,X_u,x_center=FALSE,scale=FALSE, lambda_prior=1000000, projection="semisupervised", trueprob=mean(model.matrix(~y-1,data.frame(y=y_u))[,1])) },
  "Oracle"=function(X,y,X_u,y_u) {LeastSquaresClassifier(rbind(X,X_u),unlist(list(y,y_u)),intercept=TRUE,x_center=TRUE,scale=FALSE) }
)

modelforms <- models[8]
datasets <- datasets[8]
repeats <- 100
n_labeled <- "enough"
pca <- FALSE
description <-"testing"
verbose <- TRUE

## Calculate Learning Curves
errorcurves <- lapply(names(datasets),function(dname){
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
              
							LearningCurveSSL(X,y,classifiers,n_l=n_l,s=2^(0:10),repeats=repeats, verbose=verbose, with_replacement = FALSE, n_test = 1000)
							})

names(errorcurves) <- names(datasets)

total_time <- Sys.time() - start_time 
print(total_time)

dir.create("data/",showWarnings = FALSE)
save(errorcurves,classifiers,repeats,n_labeled,total_time,file=paste0("Data/learningcurves-",repeats,"repeats-",n_labeled,"labeled-",description,".RData"))
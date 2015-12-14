library(RSSL)
library(createdatasets)
library(randomForest)
library(parallel)

## Settings
repeats <- 20
n_labeled <- "enough"
description <- "newtry"

## Load datasets
setdatadir("~/Data")
datasets<-list(#"Haberman"=createHaberman(),
               "Ionosphere"=createIonosphere(),
               "Parkinsons"=createParkinsons(),
               "Diabetes"=na.roughfix(createDiabetes()),
               "Sonar"=createSonar(),
               "SPECT"=createSPECT(),
               #"SPECTF"=createSPECTF(),
               "Transfusion"=createTransfusion(),
               "WDBC"=createWDBC(),
               #"Mammography"=na.roughfix(createMammographicMass()),
               #"Spambase"=createSpambase(),
               #"Thoraric"=createThoraricSurgery(),
               #"EEG Eye"=createEEGEye(),
               #"Banknote"=createBanknote(),
               #"Fertility"=createFertility(),
               "Digit1"=createDigit1(),
               "USPS"=createUSPS(),
               "COIL2"=createCOIL2(),
               "BCI"=createBCI(),
               #"g241c"=createG241C(),
               "g241d"=createG241N())

models <- list(#"Haberman"=formula(Survival~.),
               "Ionosphere"=formula(Return~.),
               "Parkinsons"=formula(status~ . -subject -recording),
               "Diabetes"=formula(Diabetes~.),
               "Sonar"=formula(Label ~ .),
               "SPECT"=formula(Diagnosis ~ .),
               #"SPECTF"=formula(Diagnosis ~ .),
               "Transfusion"=formula(Donated ~ .),
               "WDBC"=formula(Diagnosis ~ . -ID),
               #"Mammography"=formula(Severity ~ . -BIRADS),
               #"Spambase"=formula(Class ~ .),
               #"Thoraric"=formula(Risk1Yr ~ .),
               #"EEG Eye"=formula(eyeDetection ~ .),
               #"Banknote"=formula(Class ~ .),
               #"Fertility"=formula(Diagnosis ~ .),
               "Digit1"=formula(Class ~ .),
               "USPS"=formula(Class ~ .),
               "COIL2"=formula(Class ~ .),
               "BCI"=formula(Class ~ .),
               #"g241c"=formula(Class ~ .),
               "g241d"=formula(Class ~ .))

models <- list("SPECTF"=formula(Diagnosis ~ .))
datasets <- list("SPECTF"=createSPECTF())

datasets <- lapply(datasets,data.frame)
datasets <- datasets
modelforms <- models

## List of classifiers
classifiers<-list(
  "Supervised"=function(X,y,X_u,y_u) {LeastSquaresClassifier(X,y,intercept=TRUE,x_center=TRUE,scale=FALSE) },
  "Self-Learning"=function(X,y,X_u,y_u) {SelfLearning(X, y, X_u, method=LeastSquaresClassifier,intercept=TRUE,x_center=TRUE,scale=FALSE)},
  "ICLS"=function(X,y,X_u,y_u) {ICLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE,scale=FALSE) },
  "Oracle"=function(X,y,X_u,y_u) {LeastSquaresClassifier(rbind(X,X_u),unlist(list(y,y_u)),intercept=TRUE,x_center=TRUE,scale=FALSE) }
)

cvresults<-lapply(names(datasets),function(dname){
  cat(dname,"\n");
  classname<-all.vars(modelforms[[dname]])[1]
  data <- data.frame(datasets[[dname]]) 
  
  if (nrow(data)>2000) {
    indices<-sample(1:nrow(data),2000)
    data <- data[indices, ]
  }
  
  X <- model.matrix(modelforms[[dname]],data)
  X <- X[,colnames(X)!="(Intercept)"]
  X <- X[,apply(X, 2, var, na.rm=TRUE) != 0] # Remove constant columns
  X <- scale(X) # Pre-scale data
  
#   t_pca <- princomp(X)
#   n_comp <- sum(cumsum(t_pca$sdev^2)/sum(t_pca$sdev^2)<0.99)
#   n_comp <- n_comp #min(c(n_comp,floor(n_labeled/2)))
#   X <- t_pca$scores[,1:n_comp]
#   
  y <- data[,classname]
  
  if (n_labeled=="enough") { n_l <- max(ncol(X)+5,20) }
  else if (n_labeled=="d") { n_l <- ncol(X)+1 }
  else if (n_labeled=="2d") { n_l <- ncol(X)*2 }
  else { n_l<-n_labeled }
  
  print(mean(
    bootstrap::crossval(X, y,
                        LeastSquaresClassifier,
                        predict,ngroup=10)$cv.fit==as.numeric(y)))
  browser()
	CrossValidationSSL(X, y,
	                    classifiers,
	                    n_labeled=n_l,
	                    k=10,
	                    repeats=repeats,
	                    dataset_name=dname,
	                    verbose=T)
})

## Save results
dir.create("Data/",showWarnings = FALSE)
save.image(paste0("Data/crossvalidation-",repeats,"repeats-",n_labeled,"labeled-",description,".Rdata"))

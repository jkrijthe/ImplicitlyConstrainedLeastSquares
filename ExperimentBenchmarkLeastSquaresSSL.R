# ExperimentBenchmarkLeastSquaresSSL
library(devtools)
load_all("~/Dropbox/Code/RSSL")
library(reshape)
library(ggplot2)
require(gridExtra)
library(compiler)

## Load data
load("~/Data/Datasets.RData")
datasets<-datasets[1:16]
repeats<-10

## Settings
classifiers<-c(function(X,y,X_u,y_u) {LeastSquaresClassifier(X,y,intercept=TRUE,x_center=TRUE) },
               function(X,y,X_u,y_u) {ICLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE) },
               function(X,y,X_u,y_u) {SelfLearning(X, y, X_u, method=LeastSquaresClassifier,intercept=TRUE,x_center=TRUE)},
               function(X,y,X_u,y_u) {UCLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE) },
               function(X,y,X_u,y_u) {LeastSquaresClassifier(rbind(X,X_u),c(y,y_u),intercept=TRUE,x_center=TRUE) }
               # function(X,y,X_u,y_u) { 
               #      cl<-function(X,y,s) {LeastSquaresClassifier(X,y,intercept=TRUE,x_center=TRUE,lambda=s) }
               #      selected<-CVSelection(X,y,cl,parameters=10^(-5:5))
               #      # print(selected)
               #      LeastSquaresClassifier(X,y,intercept=TRUE,x_center=TRUE,lambda=selected)
               # },
               # function(X,y,X_u,y_u) { 
               #      cl<-function(X,y,s) {LeastSquaresClassifier(X,y,intercept=TRUE,x_center=TRUE,lambda=s) }
               #      selected<-CVSelection(X,y,cl,parameters=10^(-5:5))
               #      # print(selected)
               #      ICLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE,lambda1=selected,lambda2=selected)
               # },
               # # function(X,y,X_u,y_u) { 
               # #      cl<-function(X,y,s) {ICLeastSquaresClassifier(X,y,X_u=X_u,intercept=TRUE,x_center=TRUE,lambda1=s,lambda2=s) }
               # #      selected<-CVSelection(X,y,cl,parameters=10^(-5:5))
               # #      print(selected)
               # #      ICLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE,lambda1=selected,lambda2=selected)
               # # },
               # function(X,y,X_u,y_u) { 
               #      cl<-function(X,y,s) {LeastSquaresClassifier(X,y,intercept=TRUE,x_center=TRUE,lambda=s) }
               #      selected<-CVSelection(X,y,cl,parameters=10^(-5:5))
               #      # print(selected)
               #      SelfLearning(X, y, X_u, method=LeastSquaresClassifier,intercept=TRUE,x_center=TRUE,lambda=selected)
               # },
               # function(X,y,X_u,y_u) { 
               #      cl<-function(X,y,s) {LeastSquaresClassifier(X,y,intercept=TRUE,x_center=TRUE,lambda=s) }
               #      selected<-CVSelection(X,y,cl,parameters=10^(-5:5))
               #      # print(selected)
               #      UCLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE,lambda=selected)
               # }
               )


## SSL Type 1
# cat("SSL Type 1\n")
# cvresults<-lapply(names(datasets),function(dname){
#                                    cat(dname,"\n");
#                                    classname<-all.vars(modelforms[[dname]])[1]
#                                    current_data<-SSLDataFrameToMatrices(modelforms[[dname]],datasets[[dname]],intercept=FALSE)
#                                    n_pos<-max(ncol(current_data$X)+5,20)
#                                    print(mean(bootstrap::crossval(current_data$X,current_data$y,LeastSquaresClassifier,predict,ngroup=10)$cv.fit==current_data$y))
#                                    CrossValidationSSL(current_data$X,current_data$y,classifiers,n_labeled=n_pos,k=min(floor(nrow(current_data$X)/n_pos),10),prop_unlabeled=0.9,repeats=10,verbose=T)})



## SSL Type 2
cat("SSL Type 2\n")
cvresults<-lapply(names(datasets),function(dname){
							classname<-all.vars(modelforms[[dname]])[1]
							current_data<-SSLDataFrameToMatrices(modelforms[[dname]],datasets[[dname]],intercept=FALSE)
                                   n_pos<-max(ncol(current_data$X)+5,20)
                                   print(mean(bootstrap::crossval(current_data$X,current_data$y,LeastSquaresClassifier,predict,ngroup=10)$cv.fit==current_data$y))
							CrossValidationSSL2(current_data$X,current_data$y,classifiers,n_labeled=n_pos,k=10,repeats=repeats,dataset_name=dname,verbose=T)}
                                   )



# Transductive 
cvresults<-lapply(names(datasets),function(dname){
                                   cat(dname,"\n");
                                   classname<-all.vars(modelforms[[dname]])[1]
                                   current_data<-SSLDataFrameToMatrices(modelforms[[dname]],datasets[[dname]],intercept=FALSE)
                                   n_pos<-max(ncol(current_data$X)+5,20)
                                   CrossValidationTransductive(current_data$X,current_data$y,classifiers,n_labeled=n_pos,k=min(floor(nrow(current_data$X)/n_pos),10),repeats=10,verbose=T)})

# Save results
save.image(paste("~/Dropbox/Results/Benchmark-",repeats,"rep-justenoughplus5or20.RData",sep=""))

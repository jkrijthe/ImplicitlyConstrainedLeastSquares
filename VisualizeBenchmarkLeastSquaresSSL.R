load("~/Data/Datasets.RData")
datasets<-datasets[1:16]
load("Benchmark-100rep-justenoughplus5or20.RData")

## Print results
table.CrossValidation<-function(object,caption="",classifier_names=NULL) {
  # overfolds<-apply(object$results,c(1,3:4),mean,na.rm=T)
  if (is.list(object)) {
    if ("results" %in% names(object)) {
      object<-list(object)
    }
  } else {
    stop("Supplied object is not a cross-validation results object")
  }

  if (is.null(classifier_names)) {
    classifier_names<-dimnames(object[[1]]$results)[[2]]
  }

  cat("\\begin{table}\n")
  cat("\\caption{",caption,"} \\label{table:cvresults}\n",sep="")
  cat("\\begin{tabular}{l|",paste(rep("l",dim(object[[1]]$results)[2]),collapse=""),"}\n",sep="")
  
  cat("Dataset &",paste(classifier_names,collapse=" & "),"\\\\ \n")
  cat("\\hline\n")
  sapply(1:length(datasets), function(n) { 
    cat(object[[n]]$dataset_name,"")
    overfolds<-object[[n]]$results
    means<-apply(overfolds,c(2:3),mean,na.rm=T)
    sds<-apply(overfolds,2:3,sd)
    options(digits=2)
  for (c in 1:dim(means)[1]) {
     csd<-sprintf("%.2f",sds[c,1])
     cm<-sprintf("%.2f",1-means[c])
     make_bold <- (t.test(overfolds[,1,1],overfolds[,c,1])$p.value<0.05)&all(means[c]>=means[1])&(all(c!=c(1,5)))
     make_underlined <- all(means[c]>=means[2:4])&(t.test(overfolds[,c,1],overfolds[,which.max(means[2:4]),1])$p.value<0.05)&(all(c!=c(1,5)))
     cat("& $",ifelse(make_bold,"\\mathbf{",""),ifelse(make_underlined,"\\underline{",""), cm, " \\pm ",csd,ifelse(make_underlined,"}",""),ifelse(make_bold,"} $","$"),sep="")
  }
  cat("\\\\ \n")
  })
  cat("\\end{tabular}\n")
  cat("\\end{table}\n")
}
options(digits=2)
capture.output(table.CrossValidation(cvresults,caption="Average 10-fold cross-validation error and standard deviation over 10 repeats. The classifiers that have been compared are supervised Least Squares (LS), Implicitly constrained least squares (ICLS), self-learned least squares (SLLS), updated covariance least squares (UCLS, see text) and for comparison a supervised least squares classifier that has access to all the labels (LSoracle). Indicated in $\\mathbf{bold}$ is whether a semi-supervised classifier significantly outperform the supervised LS classifier, as measured using a $t$-test with a $0.05$ significance level. \\underline{Underlined} indicates whether a semi-supervised classifier is (significantly) best among the three semi-supervised classifiers considered.",classifier_names=c("LS","ICLS","SLLS","UCLS","LSoracle")),file="cvtable2.tex")
library(randomForest)
library(dplyr)
library(xtable)

source("code/datasets-and-classifiers.R")

dataset_properties <- function(dataset, model) {
  mm <- model.matrix(model,dataset)
  mm <- mm[,colnames(mm)!="(Intercept)"]
  mm <- mm[,apply(mm, 2, var, na.rm=TRUE) != 0]
  mf <- model.frame(model,dataset)[,-1]
  mf <- mf[,apply(mf,2,function(x) {length(unique(x))}) >1]
  pca <- princomp(scale(mm))
  return(
    list(
      Objects = nrow(dataset),
      #"Cat." = as.integer(sum(as.numeric(lapply(mf,is.factor)))),
      #"Num." = as.integer(sum(as.numeric(lapply(mf,is.numeric)))),
      Features = ncol(mm),
      PCA99 = min(which(cumsum(pca$sdev^2/sum(pca$sdev^2))>0.99)),
      "Majority" = max(prop.table(table(model.frame(model,dataset)[[1]]))),
      Source = "Unknown"
      )
  )
}

table <- lapply(names(datasets),
       function(name) {
         dataset_properties(datasets[[name]],
                            models[[name]])
         }) %>%
  bind_rows

table$Dataset <- paste0("\\textsc{",names(datasets),"}")
table$Source <- c(rep("\\cite{Bache2013}",10),rep("\\cite{Chapelle2006}",6))
table <- table[,c("Dataset",setdiff(names(table),"Dataset"))]

# Write to file
print(
  xtable(table, align="llrrrrr",
         caption="Description of the datasets used in the experiments. PCA99, refers to the number of principal components required to retain at least 99\\% of the variance. Majority refers to the proportion of the number of objects from the largest class",
         label="table:datasets"),
  file = "icls-long/table-data-properties.tex",
  sanitize.text.function=function(x) {x},
  include.rownames=FALSE,
  caption.placement="top",
) 

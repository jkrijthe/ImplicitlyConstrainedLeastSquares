load("data/crossvalidation-20repeats-enoughlabeled-.Rdata")
cvr <- cvresults
load("data/crossvalidation-20repeats-enoughlabeled-ext.Rdata")
cvresults <- c(cvr,cvresults)
cvresults<- cvresults[-6] # Remove incorrectly processed dataset

## Visualize
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)
library(xtable)

df_res <- 1:length(cvresults) %>% lapply(function(x) {cvresults[[x]]$results %>% melt %>% mutate(Dataset=cvresults[[x]]$dataset_name)}) %>% bind_rows
colnames(df_res) <- c("repeat","Classifier","Measure","Value","Dataset")

df_table <- df_res %>% 
  group_by(Classifier,Dataset,Measure) %>% 
  summarize(mean=mean(Value)) %>% 
  ungroup %>% 
  filter(Measure=="Error") %>% 
  mutate(Measure=NULL,mean=1-mean) %>%
  filter(Classifier!="USM") %>% 
  spread(Classifier,mean) %>% 
  as.data.frame

df_sd <- df_res %>% 
  group_by(Classifier,Dataset,Measure) %>% 
  summarize(sd=sd(Value)) %>% 
  ungroup %>% 
  filter(Measure=="Error") %>% 
  mutate(Measure=NULL) %>%
  filter(Classifier!="USM") %>% 
  spread(Classifier,sd) %>% 
  as.data.frame

df_ttest <- df_res %>%
  group_by(Dataset,Measure,`repeat`) %>% 
  mutate(Value=Value-Value[Classifier=="Supervised"]) %>% 
  ungroup %>% 
  group_by(Dataset,Measure,Classifier) %>% 
  summarize(ttest=(t.test(Value)$p.value<0.05 & Value<0)) %>% 
  ungroup %>% 
  filter(Measure=="Error") %>% 
  mutate(Measure=NULL) %>%
  filter(Classifier!="USM") %>% 
  spread(Classifier,ttest) %>% 
  as.data.frame


df_wilcsup <- df_res %>%
  group_by(Dataset,Measure,`repeat`) %>% 
  mutate(Value=Value-Value[Classifier=="Supervised"]) %>% 
  ungroup %>% 
  group_by(Dataset,Measure,Classifier) %>% 
  summarize(ttest=(wilcox.test(Value)$p.value<0.01 && mean(Value)<0)) %>% 
  ungroup %>% 
  filter(Measure=="Error") %>% 
  mutate(Measure=NULL) %>%
  filter(Classifier!="USM") %>% 
  spread(Classifier,ttest) %>% 
  as.data.frame

df_wilc <- df_res %>%
  group_by(Dataset,Measure,`repeat`) %>% 
  mutate(Value=Value-Value[Classifier=="Self-Learning"]) %>% 
  ungroup %>% 
  group_by(Dataset,Measure,Classifier) %>% 
  summarize(ttest=(wilcox.test(Value)$p.value<0.01&& mean(Value)>0)) %>% 
  ungroup %>% 
  filter(Measure=="Error") %>% 
  mutate(Measure=NULL) %>%
  filter(Classifier!="USM") %>% 
  spread(Classifier,ttest) %>% 
  as.data.frame
  
df_bigger <- df_res %>%
  group_by(Dataset,Measure,`repeat`) %>% 
  mutate(Value=Value-Value[Classifier=="Supervised"]) %>% 
  ungroup %>% 
  group_by(Dataset,Measure,Classifier) %>% 
  summarize(bigger=mean(Value)<0) %>% 
  ungroup %>% 
  filter(Measure=="Error") %>% 
  mutate(Measure=NULL) %>%
  filter(Classifier!="USM") %>% 
  spread(Classifier,bigger) %>% 
  as.data.frame

df_often <- df_res %>%
  group_by(Dataset,Measure,`repeat`) %>% 
  mutate(Value=Value-Value[Classifier=="Supervised"]) %>% 
  ungroup %>% 
  group_by(Dataset,Measure,Classifier) %>% 
  summarize(bigger=sum(Value<0)) %>% 
  ungroup %>% 
  filter(Measure=="Error") %>% 
  mutate(Measure=NULL) %>%
  filter(Classifier!="USM") %>% 
  spread(Classifier,bigger) %>% 
  as.data.frame



format_cells <- function(number,secondnumber,bold,underscore,skipcol=2)  {
  out <- number
  for (i in 1:nrow(out)) {
    out[i,1] <- paste("\\texttt{",out[i,1],"}")
    for (j in (1+skipcol):(ncol(out)-1)) {
      out[i,j] <- sprintf("%.2f (%d)",number[i,j],secondnumber[i,j])
      if (!is.na(bold[i,j]) & bold[i,j]) { out[i,j] <- paste("\\textbf{",out[i,j],"}",sep="") }
      if (!is.na(underscore[i,j]) & underscore[i,j]==TRUE) { out[i,j] <- paste("\\underline{",out[i,j],"}",sep="") }
    }
  }
  out
}

df_formatted <- format_cells(df_table,df_often,df_wilc,df_wilcsup)
df_formatted<-df_formatted[c(6,7,3,8,9,10,12,4,11,2,1,5),]
print(xtable(df_formatted),include.rownames=FALSE,sanitize.text.function=identity, file="figure/table-crossvalidation.tex")

library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)
library(xtable)

load("data/crossvalidation-100repeats-enoughlabeled-final.RData")

format_cell <- function(number,bold,underscore, brackets) {
  out <- sprintf("%.2f (%d)",number,brackets)
  if (!(all(bold==FALSE))) {
    out <- ifelse(!is.na(bold) & bold, paste0("\\textbf{",out,"}"),out)
  }
  if (!(all(underscore==FALSE))) {
    out <- ifelse(!is.na(underscore) & underscore, paste0("\\underline{",out,"}"),out) 
  }
  out[1] <- sprintf("%.2f",number)
  out
}

df_res <- 1:length(cvresults) %>% 
  lapply(function(x) {
    cvresults[[x]]$results %>% 
      mutate(Dataset=names(cvresults)[x])}) %>% 
  bind_rows %>% 
  rename(Value=value)

df_res$Dataset <- factor(df_res$Dataset,
                             names(cvresults),
                             paste0("\\textsc{",names(cvresults),"}"))

class_names <- levels(df_res$Classifier)
class_names[1:2] <- c("Sup","Self")

df_publish <- df_res %>%
  group_by(Dataset,Measure,`repeats`,Classifier) %>% 
  summarize(Value=mean(Value)) %>%
  ungroup %>% 
  group_by(Dataset, Measure, `repeats`) %>% 
  mutate(ValueDiff=Value-Value[Classifier=="Supervised"]) %>% 
  ungroup %>% 
  group_by(Dataset,Measure,Classifier) %>% 
  summarize(
    mean=mean(Value),
    ttest=(wilcox.test(ValueDiff)$p.value<0.01 && mean(ValueDiff)>0),
    bigger=sum(ValueDiff>0)
    ) %>%
  mutate(best=(mean==min(mean[Classifier!="Oracle"]))) %>% 
  mutate(out=format_cell(mean, best, ttest, bigger)) %>%
  select(-mean,-ttest,-bigger,-best) %>% 
  ungroup %>% 
  filter(Measure=="Error") %>%
  spread(Classifier,out) %>%
  select(-Measure) %>% 
  as.data.frame
print(df_publish)



print(xtable(df_publish),include.rownames=FALSE,sanitize.text.function=identity, only.contents=TRUE, file="figures/table-crossvalidation-long.tex")

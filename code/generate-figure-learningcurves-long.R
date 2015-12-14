library(RSSL)

load("data/learningcurves-1000repeats-enoughlabeled-combined.Rdata")

## Visualize
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)

df_res <- 1:length(errorcurves) %>% lapply(function(x) {errorcurves[[x]]$results  %>% mutate(Dataset=paste0("\\textsc{",names(errorcurves)[x],"}"))}) %>% bind_rows

#levels(df_res$Classifier) <- levels(df_res$Classifier)
#df_res$Classifier <- factor(df_res$Classifier, levels=names(classifiers))

p_error <- df_res  %>%
  group_by(`Number of unlabeled objects`,Classifier,Measure,Dataset) %>%
  summarize(Mean=mean(value),SE=sd(value)/sqrt(n())) %>% 
  ungroup %>%
  filter(Measure %in% c("Error")) %>% 
  #filter(Classifier %in% c("Supervised","Self-Learning","ICLS")) %>% 
  filter(Classifier!="ICLS_prior") %>% 
  filter(Classifier!="Oracle") %>% 
  ggplot(aes(x=`Number of unlabeled objects`,y=Mean,color=Classifier,shape=Classifier)) +
  geom_point(size=2) +
  geom_line(aes(linetype=Classifier)) +
  geom_ribbon(aes(ymax=Mean+1*SE,ymin=Mean-1*SE,fill=Classifier),size=0,alpha=0.4,color=0) +
  #geom_errorbar(aes(ymax=Mean+2*SE,ymin=Mean-2*SE,fill=Classifier),width=0.1) +
  scale_x_continuous(trans = log2_trans()) +
  scale_color_manual(values=c("grey","#8dd3c7","#bebada","#fb8072")) +
  scale_fill_manual(values=c("grey","#8dd3c7","#bebada","#fb8072")) +
  theme_classic() +
  facet_wrap(~ Dataset,scales="free",ncol=4) +
  xlab("Unlabeled objects")+
  ylab("Error") +
  theme(legend.position="bottom",strip.background=element_rect(size = 0),axis.title.y=element_text(angle = 0,size=rel(0.8)),axis.title.x=element_text(size=rel(0.8)),axis.text.y=element_text(size=rel(0.8)),axis.text.x=element_text(size=rel(0.8)))
print(p_error)

p_loss <- df_res  %>%
  group_by(`Number of unlabeled objects`,Classifier,Measure,Dataset) %>%
  summarize(Mean=mean(value),SE=sd(value)/sqrt(n())) %>% 
  ungroup %>%
  filter(Measure %in% c("Loss Test")) %>% 
  #filter(Classifier %in% c("Supervised","Self-Learning","ICLS")) %>% 
  filter(Classifier!="ICLS_prior") %>% 
  filter(Classifier!="Oracle") %>% 
  ggplot(aes(x=`Number of unlabeled objects`,y=Mean,color=Classifier,shape=Classifier)) +
  geom_point(size=2) +
  geom_line(aes(linetype=Classifier)) +
  geom_ribbon(aes(ymax=Mean+1*SE,ymin=Mean-1*SE,fill=Classifier),size=0,alpha=0.4,color=0) +
  #geom_errorbar(aes(ymax=Mean+2*SE,ymin=Mean-2*SE,fill=Classifier),width=0.1) +
  scale_x_continuous(trans = log2_trans()) +
  scale_color_manual(values=c("grey","#8dd3c7","#bebada","#fb8072")) +
  scale_fill_manual(values=c("grey","#8dd3c7","#bebada","#fb8072")) +
  theme_classic() +
  facet_wrap(~ Dataset,scales="free",ncol=4) +
  xlab("Unlabeled objects")+
  ylab("Squared\nLoss\nTest Set") +
  theme(legend.position="bottom",strip.background=element_rect(size = 0),axis.title.y=element_text(angle = 0,size=rel(0.8)),axis.title.x=element_text(size=rel(0.8)),axis.text.y=element_text(size=rel(0.8)),axis.text.x=element_text(size=rel(0.8)))
print(p_loss)

p_time <- df_res  %>%
  group_by(`Number of unlabeled objects`,Classifier,Measure,Dataset) %>%
  summarize(Mean=mean(value),SE=sd(value)/sqrt(n())) %>% 
  ungroup %>%
  filter(Measure %in% c("Time")) %>% 
  #filter(Classifier %in% c("Supervised","Self-Learning","ICLS")) %>% 
  filter(Classifier!="ICLS_prior") %>% 
  filter(Classifier!="Oracle") %>% 
  ggplot(aes(x=`Number of unlabeled objects`,y=Mean,color=Classifier,shape=Classifier)) +
  geom_point(size=2) +
  geom_line(aes(linetype=Classifier)) +
  geom_ribbon(aes(ymax=Mean+1*SE,ymin=Mean-1*SE,fill=Classifier),size=0,alpha=0.4,color=0) +
  #geom_errorbar(aes(ymax=Mean+2*SE,ymin=Mean-2*SE,fill=Classifier),width=0.1) +
  scale_x_continuous(trans = log2_trans()) +
  scale_y_continuous(trans = log2_trans(),labels=format_format(digits=2)) +
  scale_color_manual(values=c("grey","#8dd3c7","#bebada","#fb8072")) +
  scale_fill_manual(values=c("grey","#8dd3c7","#bebada","#fb8072")) +
  theme_classic() +
  facet_wrap(~ Dataset,scales="free",ncol=4) +
  xlab("Unlabeled objects")+
  ylab("Time\n($\\log_2$ s)") +
  theme(legend.position="bottom",strip.background=element_rect(size = 0),axis.title.y=element_text(angle = 0,size=rel(0.8)),axis.title.x=element_text(size=rel(0.8)),axis.text.y=element_text(size=rel(0.8)),axis.text.x=element_text(size=rel(0.8)))
print(p_time)

library(tikzDevice)
tikz(file = "figures/figure-errorcurves-long.tex",height="4.5",width="6.5",onefile=FALSE)
print(p_error)
dev.off()

tikz(file = "figures/figure-losscurves-long.tex",height="4.5",width="6.5")
print(p_loss)
dev.off()

tikz(file = "figures/figure-timecurves-long.tex",height="4.5",width="6.5")
print(p_time)
dev.off()
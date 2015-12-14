library(RSSL)

load("data/learningcurves-100repeats-enoughlabeled-scale.RData")

## Visualize
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)

errorcurves <- errorcurves[lapply(errorcurves,class)=="ErrorCurve"]

df_res <- 1:length(errorcurves) %>% lapply(function(x) {errorcurves[[x]]$results %>% melt %>% mutate(Dataset=names(errorcurves)[x])}) %>% bind_rows
colnames(df_res) <- c("repeat","Objects","Classifier","Measure","Value","Dataset")
#levels(df_res$Classifier) <- levels(df_res$Classifier)
#df_res$Classifier <- factor(df_res$Classifier, levels=names(classifiers))
p <- df_res %>%
  group_by(Objects,Classifier,Measure,Dataset) %>%
  summarize(Mean=mean(Value),SE=sd(Value)/sqrt(n())) %>% 
  ungroup %>%
  filter(Measure %in% c("Error")) %>% 
  filter(!(Dataset %in% c("Haberman","g241c","Transfusion","Mammography"))) %>% 
  filter(Classifier %in% c("Supervised","Self-Learning","ICLS")) %>% 
  #filter(Classifier!="Oracle") %>% 
  ggplot(aes(x=Objects,y=Mean,color=Classifier,shape=Classifier)) +
  geom_point(size=2) +
  geom_line(aes(linetype=Classifier)) +
  geom_ribbon(aes(ymax=Mean+1*SE,ymin=Mean-1*SE,fill=Classifier),size=0,alpha=0.4,color=0) +
  #geom_errorbar(aes(ymax=Mean+2*SE,ymin=Mean-2*SE,fill=Classifier),width=0.1) +
  scale_x_continuous(trans = log2_trans()) +
  scale_color_manual(values=c("#8dd3c7","#80b1d3","#fb8072")) +
  scale_fill_manual(values=c("#8dd3c7","#80b1d3","#fb8072")) +
  theme_classic() +
  facet_wrap(~ Dataset,scales="free",ncol=4) +
  xlab("Unlabeled objects")+
  ylab("Error") +
  theme(legend.position="bottom",strip.background=element_rect(size = 0),axis.title.y=element_text(angle = 0,size=rel(0.8)),axis.title.x=element_text(size=rel(0.8)),axis.text.y=element_text(size=rel(0.8)),axis.text.x=element_text(size=rel(0.8)))
p

library(tikzDevice)
tikz(file = "figures/figure-errorcurves.tex",height="4.5",width="6.5")
p
dev.off()

#pdf(file=paste0("Data/learningcurves-",repeats,"repeats-",n_labeled,"labeled-",description,".pdf"),width=10,height=6)
print(p)
#dev.off()
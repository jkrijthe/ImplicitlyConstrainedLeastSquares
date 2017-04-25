library(RSSL)

## Visualize
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)

load("data/learningcurves-1000repeats-10labeled-peaking.RData")

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
  #filter(Classifier!="Oracle") %>% 
  ggplot(aes(x=`Number of unlabeled objects`,y=Mean,color=Classifier,shape=Classifier)) +
  geom_point(size=2) +
  geom_line(aes(linetype=Classifier)) +
  geom_ribbon(aes(ymax=Mean+1*SE,ymin=Mean-1*SE,fill=Classifier),size=0,alpha=0.4,color=0) +
  #geom_errorbar(aes(ymax=Mean+2*SE,ymin=Mean-2*SE,fill=Classifier),width=0.1) +
  #scale_x_continuous(trans = log2_trans()) +
  scale_color_manual(values=c("grey","#8dd3c7","#bebada","#fb8072","yellow")) +
  scale_fill_manual(values=c("grey","#8dd3c7","#bebada","#fb8072","yellow")) +
  theme_classic() +
  scale_x_continuous(breaks=seq(0,150,50),minor_breaks=seq(0,150,10),expand = c(0,0),limits=c(0,150)) +
  facet_wrap(~ Dataset,scales="free",ncol=4) +
  xlab("Unlabeled objects")+
  ylab("Error") +
  theme(legend.position="bottom",
    strip.background=element_rect(size = 0),
    axis.title.y=element_text(angle = 0,size=rel(0.8)),
    axis.title.x=element_text(size=rel(0.8)),
    axis.text.y=element_text(size=rel(0.8)),
    axis.text.x=element_text(size=rel(0.8)),
    axis.line.x=element_line(size=0.8 ,color="black",linetype=1),
    axis.line.y=element_line(size=0.8 ,color="black",linetype=1),
    panel.grid.major.x=element_line(colour = "darkgrey",size = 0.5,linetype = "dotted"),
    panel.grid.minor.x=element_line(colour = "darkgrey",size = 0.5,linetype = "dotted"),
    panel.grid.major.y=element_line(colour = "darkgrey",size = 0.5,linetype = "dotted"),
    panel.grid.minor.y=element_line(colour = "darkgrey",size = 0.5,linetype = "dotted")
  ) +
  labs(fill="",color="",linetype="",shape="")


p_error_zoom <- df_res  %>%
  group_by(`Number of unlabeled objects`,Classifier,Measure,Dataset) %>%
  summarize(Mean=mean(value),SE=sd(value)/sqrt(n())) %>% 
  ungroup %>%
  filter(`Number of unlabeled objects`<=25) %>% 
  filter(Measure %in% c("Error")) %>% 
  #filter(Classifier %in% c("Supervised","Self-Learning","ICLS")) %>% 
  filter(Classifier!="ICLS_prior") %>% 
  #filter(Classifier!="Oracle") %>% 
  ggplot(aes(x=`Number of unlabeled objects`,y=Mean,color=Classifier,shape=Classifier)) +
  geom_point(size=2) +
  geom_line(aes(linetype=Classifier)) +
  geom_ribbon(aes(ymax=Mean+1*SE,ymin=Mean-1*SE,fill=Classifier),size=0,alpha=0.4,color=0) +
  #geom_errorbar(aes(ymax=Mean+2*SE,ymin=Mean-2*SE,fill=Classifier),width=0.1) +
  #scale_x_continuous(trans = log2_trans()) +
  scale_color_manual(values=c("grey","#8dd3c7","#bebada","#fb8072","yellow")) +
  scale_fill_manual(values=c("grey","#8dd3c7","#bebada","#fb8072","yellow")) +
  scale_x_continuous(breaks=seq(0,50,5),minor_breaks=seq(0,50,1),expand = c(0,0),limits=c(0,25))+
  theme_classic() + 
  theme(legend.position="bottom",
          strip.background=element_rect(size = 0, fill=NA),
          axis.line.x=element_line(size=0.8 ,color="black",linetype=1),
          axis.line.y=element_line(size=0.8 ,color="black",linetype=1),
          axis.ticks=element_line(colour = "black",size = 0.5),
          axis.title.y=element_text(angle = 0,size=rel(0.8)),
          panel.background=element_rect(fill=NA),
          panel.grid.major.x=element_line(colour = "darkgrey",size = 0.5,linetype = "dotted"),
          panel.grid.minor.x=element_line(colour = "darkgrey",size = 0.5,linetype = "dotted"),
          panel.grid.major.y=element_line(colour = "darkgrey",size = 0.5,linetype = "dotted"),
          panel.grid.minor.y=element_line(colour = "darkgrey",size = 0.5,linetype = "dotted"),
          axis.title.x=element_text(size=rel(0.8)),
          axis.text.y=element_text(size=rel(0.8)),
          axis.text.x=element_text(size=rel(0.8))) +
  facet_wrap(~ Dataset,scales="free",ncol=4) +
  xlab("Unlabeled objects") +
  ylab("Error") +
  labs(fill="",color="",linetype="",shape="")
  
# p_loss <- df_res  %>%
#   group_by(`Number of unlabeled objects`,Classifier,Measure,Dataset) %>%
#   summarize(Mean=mean(value),SE=sd(value)/sqrt(n())) %>% 
#   ungroup %>%
#   filter(Measure %in% c("Loss Test")) %>% 
#   #filter(Classifier %in% c("Supervised","Self-Learning","ICLS")) %>% 
#   filter(Classifier!="ICLS_prior") %>% 
#   #filter(Classifier=="Oracle") %>% 
#   ggplot(aes(x=`Number of unlabeled objects`,y=Mean,color=Classifier,shape=Classifier)) +
#   geom_point(size=2) +
#   geom_line(aes(linetype=Classifier)) +
#   geom_ribbon(aes(ymax=Mean+1*SE,ymin=Mean-1*SE,fill=Classifier),size=0,alpha=0.4,color=0) +
#   #geom_errorbar(aes(ymax=Mean+2*SE,ymin=Mean-2*SE,fill=Classifier),width=0.1) +
#   #scale_x_continuous(trans = log2_trans()) +
#   scale_color_manual(values=c("grey","#8dd3c7","#bebada","#fb8072","yellow")) +
#   scale_fill_manual(values=c("grey","#8dd3c7","#bebada","#fb8072","yellow")) +
#   theme_classic() +
#   facet_wrap(~ Dataset,scales="free",ncol=4) +
#   xlab("Unlabeled objects")+
#   ylab("Loss\non\nTest") +
#   theme(legend.position="bottom",strip.background=element_rect(size = 0),axis.title.y=element_text(angle = 0,size=rel(0.8)),axis.title.x=element_text(size=rel(0.8)),axis.text.y=element_text(size=rel(0.8)),axis.text.x=element_text(size=rel(0.8)))
  #scale_y_continuous(limits=c(0,2))
#geom_vline(xintercept=12) +
#geom_vline(xintercept=20)

  
# p_train <- df_res  %>%
#   group_by(`Number of unlabeled objects`,Classifier,Measure,Dataset) %>%
#   summarize(Mean=mean(value),SE=sd(value)/sqrt(n())) %>% 
#   ungroup %>%
#   filter(Measure %in% c("Loss Labeled")) %>% 
#   #filter(Classifier %in% c("Supervised","Self-Learning","ICLS")) %>% 
#   filter(Classifier!="ICLS_prior") %>% 
#   #filter(Classifier=="Oracle") %>% 
#   ggplot(aes(x=`Number of unlabeled objects`,y=Mean,color=Classifier,shape=Classifier)) +
#   geom_point(size=2) +
#   geom_line(aes(linetype=Classifier)) +
#   geom_ribbon(aes(ymax=Mean+1*SE,ymin=Mean-1*SE,fill=Classifier),size=0,alpha=0.4,color=0) +
#   #geom_errorbar(aes(ymax=Mean+2*SE,ymin=Mean-2*SE,fill=Classifier),width=0.1) +
#   #scale_x_continuous(trans = log2_trans()) +
#   scale_color_manual(values=c("grey","#8dd3c7","#bebada","#fb8072","yellow")) +
#   scale_fill_manual(values=c("grey","#8dd3c7","#bebada","#fb8072","yellow")) +
#   theme_classic() +
#   facet_wrap(~ Dataset,scales="free",ncol=4) +
#   xlab("Unlabeled objects")+
#   ylab("Loss\non\nLabeled") +
#   theme(legend.position="bottom",strip.background=element_rect(size = 0),axis.title.y=element_text(angle = 0,size=rel(0.8)),axis.title.x=element_text(size=rel(0.8)),axis.text.y=element_text(size=rel(0.8)),axis.text.x=element_text(size=rel(0.8))) +
#   scale_y_continuous(limits=c(0,0.1))
  #geom_vline(xintercept=12) +
  #geom_vline(xintercept=20)  

library(tikzDevice)
tikz(file = "figures/figure-peaking.tex",height="3.5",width="6.5")
print(p_error)
dev.off()


tikz(file = "figures/figure-peaking-zoom.tex",height="3.5",width="6.5")
print(p_error_zoom)
dev.off()

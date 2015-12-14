library(RSSL)
library(dplyr)
library(ggplot2)

single_repeat <- function(x) {
  expected <- TRUE
  df_labeled <- generate2ClassGaussian(10, d=2, expected=expected)
  df_labeled$Class <- factor(df_labeled$Class,levels=c(-1,1),labels=c(-1,1))
  df_unlabeled <- generate2ClassGaussian(200, d=2, expected=expected) %>% filter(Class==-1)
  df_unlabeled <- generate2ClassGaussian(200, d=2, expected=expected) %>% filter(Class==-1)
  df_unlabeled$Class <- NA

  df_train <- rbind(df_labeled,df_unlabeled)
  
  df_train <- generate2ClassGaussian(20, d=2, expected=expected)
  df_train[sample(which(df_train$Class==-1),round(sum(df_train$Class==-1)*0.5)),]$Class <- NA
  df_test <- generate2ClassGaussian(10000, d=2, expected=expected)
  
  
  g_sup <- LeastSquaresClassifier(Class~.,df_train,intercept=TRUE,x_center=TRUE,scale=TRUE,y_scale=TRUE)
  g_semi <- ICLeastSquaresClassifier(Class~.,df_train,intercept=TRUE,x_center=TRUE,scale=TRUE,y_scale=TRUE)
  g_usm <- USMLeastSquaresClassifier(Class~.,df_train,intercept=TRUE,x_center=TRUE,scale=TRUE,y_scale=TRUE)
  g_self <- SelfLearning(Class~.,df_train, method=LeastSquaresClassifier,intercept=TRUE,x_center=TRUE,scale=TRUE,y_scale=TRUE)
  
  data_frame("Supervised"=1-mean(predict(g_sup,df_test)==df_test$Class),
              "ICLS"=1-mean(predict(g_semi,df_test)==df_test$Class),
              "Self-Learning"=1-mean(predict(g_self,df_test)==df_test$Class),
             "USMLS"=1-mean(predict(g_usm,df_test)==df_test$Class)
  )
}

single_repeat()
results <- bind_rows(lapply(1:100,single_repeat))
print(results %>% colMeans)

set.seed(1)
expected=TRUE
df_labeled <- generate2ClassGaussian(10, d=2, expected=expected)
df_labeled$Class <- factor(df_labeled$Class,levels=c(-1,1),labels=c(-1,1))
df_unlabeled <- generate2ClassGaussian(200, d=2, expected=expected) %>% filter(Class==-1)
df_unlabeled$Class <- NA
df_train <- rbind(df_labeled,df_unlabeled)
df_test <- generate2ClassGaussian(10000, d=2, expected=expected)

g_sup <- LeastSquaresClassifier(Class~.,df_train,
                                intercept=TRUE,x_center=TRUE,scale=FALSE,y_scale=FALSE)
g_self <- SelfLearning(Class~.,df_train, method=LeastSquaresClassifier,intercept=TRUE,x_center=FALSE,scale=FALSE,y_scale=FALSE)
g_semi <- ICLeastSquaresClassifier(Class~.,df_train,
                                   intercept=TRUE,x_center=TRUE,scale=FALSE,y_scale=FALSE)


p <- df_train %>% 
  ggplot(aes(x=X1,y=X2,shape=Class)) +
  geom_point(size=5) +
  geom_classifier("Supervised"=g_sup,"ICLS"=g_semi,"Self-Learning"=g_self@model) +
  scale_shape(solid=TRUE,na.value=1) +
  scale_x_continuous(limits = c(-3, 3)) +
  scale_y_continuous(limits = c(-3, 3)) +
  coord_equal() +
  theme_bw() 

tikz(file = "figures/figure-mnar.tex",height="4.5",width="6.5")
p
dev.off()
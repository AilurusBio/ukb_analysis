library(caTools )
Data_dictionary <- read.csv('../../Data_Dictionary_Showcase.csv')

Cognitive_id <- Data_dictionary$FieldID[grep(' Cognitive function',Data_dictionary$Path)]
Blood_id <- Data_dictionary$FieldID[grep('Blood assays',Data_dictionary$Path)]
Population_id <- Data_dictionary$FieldID[grep('Population characteristics',Data_dictionary$Path)]



P_AD_id <- as.numeric(str_split(names(P_AD),'[X\\.]',simplify = T)[,2])
unique(P_AD_id)
Blood_id_list <- paste0('X',intersect(Blood_id,P_AD_id),".0")
Blood_id_list <- sapply(Blood_id_list, function(x) grep(x,names(P_AD),value = T))
Population_id_list <- paste0('X',intersect(Population_id,P_AD_id),".0")
Population_id_list <- sapply(Population_id_list, function(x) grep(x,names(P_AD),value = T))

all_age <- as.numeric(str_split(P_AD[,c('X53.0.0')],'-',simplify = T)[,1])-
  (P_AD[,c('X34.0.0')])

# case_eid_trainK,control_eid_trainK,case_eid,control_eid,control_eid_11

common_simple <- match(control_eid,P_AD$eid)#setdiff((1:nrow(P_AD))[all_age>=60],union(foucs_id,foucs_id_MCI))
set.seed(1234)
common_simple <- match(control_eid_trainK,P_AD$eid)
AD_sample <- match(case_eid_trainK,P_AD$eid)

summary(apply(P_AD[c(AD_sample),Blood_id_list], 1, function(x) length(which(is.na(x)))))
summary(apply(P_AD[c(common_simple),Blood_id_list], 1, function(x) length(which(is.na(x)))))

df <- P_AD[c(common_simple,AD_sample),Blood_id_list]
df$label <- c(rep('control',length(common_simple)),rep('AD',length(AD_sample)))
df$label <- as.factor(df$label)
df$X23000.0.0
set.seed(1234)
train_id <- sample(1:nrow(df),round(nrow(df)*0.7,0))
test_id <- setdiff(1:nrow(df),train_id)
df_train <- df[train_id,-c(1:7,28,30)]
df_test <- df[test_id,-c(1:7,28,30)]

pre=glm(label~.,data=df_train,family=binomial(link="logit"))
predict_data=predict.glm(pre,type="response",newdata=df_test)
predict_data=ifelse(predict_data>0.5,1,0)
table(df_test$label,predict_data)


pre=glm(label~.,data=df[,-c(1:7,28,30)],family=binomial(link="logit"))
barplot(coefficients(pre)  ,las=2)
plotdata <- data.frame(label=names(coefficients(pre)),
                       Coef=coefficients(pre))
plotdata <- plotdata[abs(plotdata$Coef) > 0.5,]
library(ggplot2)
plotdata$label <- as.character(plotdata$label)
plotdata$label[2:4] <- c('Apolipoprotein B','Cystatin C','LDL direct')
ggplot(plotdata,aes(x=label,y=Coef))+geom_bar(stat="identity")


predict=ifelse(fitted.values(pre)>0.5,1,0)
acc_table <- table(df[names(predict),]$label,predict)
(acc_table['AD','1'] + acc_table['control','0'])/sum(acc_table)
(colAUC(data.frame(fitted.values(pre)), as.numeric(df[names(fitted.values(pre)),]$label)-1,
        plotROC=FALSE, alg=c("ROC")))

mean((as.numeric(df[names(predict),]$label)-1-fitted.values(pre))^2) # mse
res_list <- list()
for(i in setdiff(1:37,c(1:7,28,30)) ){
  pre=glm(label~.,data=df[,c(i,38)],family=binomial(link="logit"))
  predict=ifelse(fitted.values(pre)>0.5,1,0)
  acc_table <- table(df[names(predict),]$label,predict)
  if (ncol(acc_table)==1) {
    next()
  }
  res_list[[names(df)[i]]] <-c(acc= ((acc_table['AD','1'] + acc_table['control','0'])/sum(acc_table)),
                               auc= (colAUC(data.frame(fitted.values(pre)), as.numeric(df[names(fitted.values(pre)),]$label)-1,
                                            plotROC=FALSE, alg=c("ROC")))[1],
                               coef=coef(pre)[[2]])
}
single_lm <- t(data.frame(res_list))
single_lm <- data.frame(single_lm[single_lm[,'auc'] >0.54,])
single_lm$label <- c('Apolipoprotein B',
                     'Direct bilirubin',
                     'Cholesterol',
                     'C-reactive protein',
                     'LDL direct')
library(reshape2)
plotdata <- melt(single_lm)
plotdata$label 

ggplot(plotdata,aes(x=label ,y=value))+
  geom_bar(aes(fill=variable),stat="identity",width=0.5,position='dodge')
##############
View(names(P_1k))
label_over <- list(
  ukb44440 = as.numeric(str_split(names(P_1k),'[X\\.]',simplify = T)[,2]),
  Data_dictionary= Data_dictionary$FieldID
)
gplots::venn(label_over)


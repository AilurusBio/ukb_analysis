load('S5_model_res.RData')
code <- read.csv(paste0(rawdata_path,'./Codings.csv'))

library(caTools)
acc_res <- rbind(SNP=sapply(model_res_all$SNP, function(x) x$confusionRes$byClass)['Balanced Accuracy',],
                 Others=sapply(model_res_all$Others, function(x) x$confusionRes$byClass)['Balanced Accuracy',],
                 'Others+blood'=sapply(model_res_all$`Others+blood`, function(x) x$confusionRes$byClass)['Balanced Accuracy',],
                 'Others+blood+SNP'=sapply(model_res_all$`Others+blood+SNP`, function(x) x$confusionRes$byClass)['Balanced Accuracy',]
)
auc_res <- rbind(SNP=colAUC(sapply(model_res_all$SNP, function(x) x$predictions_prob[,1]),test_data$PHENOTYPE),
                 Others=colAUC(sapply(model_res_all$Others, function(x) x$predictions_prob[,1]),test_data$PHENOTYPE),
                 'Others+blood'=colAUC(sapply(model_res_all$`Others+blood`, function(x) x$predictions_prob[,1]),test_data$PHENOTYPE),
                 'Others+blood+SNP'=colAUC(sapply(model_res_all$`Others+blood+SNP`, function(x) x$predictions_prob[,1]),test_data$PHENOTYPE)
)

rownames(auc_res) <- c('SNP','Others','Others+blood', 'Others+blood+SNP')
auc_res_colname_ori <- colnames(auc_res)
colnames(auc_res) <- c("ANN" , "SVM-Linear", "SVM-Radial", "KNN" ,  "RandomForest" , "AdaBoost" , "Bayes-glm", "C5.0")
print(auc_res)

Others_res <- sapply(model_res_all$Others, function(x) x$predictions_prob[,1])
colnames(Others_res) <- colnames(auc_res)
OB_res <- sapply(model_res_all$`Others+blood`, function(x) x$predictions_prob[,1])
colnames(OB_res) <- colnames(auc_res)

colAUC(Others_res,test_data$PHENOTYPE,plotROC = T)
colAUC(OB_res,test_data$PHENOTYPE,plotROC = T)


best_feature <- rownames(auc_res)[which(auc_res==max(auc_res),arr.ind = T)[1]]
best_model_name <- auc_res_colname_ori[which(auc_res==max(auc_res),arr.ind = T)[2]]
best_model <- model_res_all[[best_feature]][[best_model_name]]$model
best_model_performance <- model_res_all[[best_feature]][[best_model_name]]$confusionRes
best_model_performance_AUC <- auc_res[best_feature,which(auc_res==max(auc_res),arr.ind = T)[2]]
prs_performance <- caret::confusionMatrix(lm_res$test_data$PHENOTYPE, factor(ifelse(lm_res$predict_test>0.5,1,0)))

library(gbm)
knnImp <- varImp(best_model)

FieldID_s <- sapply(str_split(rownames(knnImp$importance),'[fX\\.]'), function(x) setdiff(x,c('','0')))
#FieldID_s <- setdiff(FieldID_s,"1797")
rownames(knnImp$importance) <- label_select$Field[match(FieldID_s,label_select$FieldID)]


dotPlot(knnImp)
knnImp_res <- data.frame(Feature=rownames(knnImp$importance)[order(knnImp$importance[,1],decreasing = T)],
                         Importance=knnImp$importance[order(knnImp$importance[,1],decreasing = T),])

knnImp_res_top3 <- knnImp_res[1:3,1]

save(auc_res,best_feature,best_model_name,best_model,best_model_performance,Others_res,OB_res,test_data,file = 'S6_val_res.RData')
###########################

# 
# library(C50)
# train_data <- train_data_other
# test_data <- test_data_other
# 
# colnames(train_data)[-ncol(train_data)] <- as.character(label_select$Field[match(sapply(str_split(colnames(train_data)[-ncol(train_data)],'[fX\\.]'), function(x) setdiff(x,c('','0'))),
#                                                                                  label_select$FieldID)])
# colnames(test_data)[-ncol(test_data)] <- as.character(label_select$Field[match(sapply(str_split(colnames(test_data)[-ncol(test_data)],'[fX\\.]'), function(x) setdiff(x,c('','0'))),
#                                                                                label_select$FieldID)])
# 
# train_data$PHENOTYPE <- factor(train_data$PHENOTYPE,levels = c(1,0),labels = c('case','control'))
# test_data$PHENOTYPE <- factor(test_data$PHENOTYPE,levels = c(1,0),labels = c('case','control'))
# 
# formula <- PHENOTYPE ~ .
# 
# imputer <- preProcess(train_data, method = "medianImpute")
# imputed_data <- predict(imputer, train_data)
# test_data_imputed <- predict(imputer, test_data)
# imputed_data1 <- imputed_data
# 
# 
# mod1 <- C5.0(PHENOTYPE ~ ., data = imputed_data1,
#              trials=100,
#              rules=F,
#              control = C5.0Control(winnow = T,minCases=20)) #model_res_all$Others$C5.0$model$bestTune$winnow
# 
# 
# C5imp_id <- sapply(str_split(rownames(C5imp(mod1))[C5imp(mod1)$Overall > 0],'[Xf\\.]'),function(x) setdiff(x,c('0','')))
# 
# 
# imputed_data1_id <- sapply(str_split(colnames(imputed_data1),'[Xf\\.]'),function(x) setdiff(x,c('0','')))
# 
# i <- 1
# for (i in 1:(ncol(imputed_data1)-1)) {
#   print(i)
#   ID <- label_select$FieldID[match(imputed_data1_id,label_select$FieldID)][i]
#   if (length(grep('ategorical',label_select$ValueType[label_select$FieldID==ID]))==1) {
#     print(label_select[match(imputed_data1_id,label_select$FieldID)[i],])
#     
#     code_pre <- code[which(code$Coding==label_select$Coding[label_select$FieldID==ID]),]
#     code_pre$Meaning <- (as.character(code_pre$Meaning))
#     #code_pre$Meaning <- str_split(code_pre$Meaning,',',simplify = T)[,1]
#     code_pre$Meaning <- as.factor(gsub('[[:punct:]]','',code_pre$Meaning))
#     
#     imputed_data1[,i]  <- code_pre$Meaning[match(imputed_data1[,i],code_pre$Value)]
#     print(code_pre$Meaning)
#     #imputed_data1[,i]  <- gsub('[[:punct:]]',''
#   }
#   }
# 
# colnames(imputed_data1) <- sapply(label_select$Field[match(imputed_data1_id,label_select$FieldID)], function(x) gsub('[[:punct:]]','',as.character(x)))
# colnames(imputed_data1)[length(colnames(imputed_data1))] <- 'PHENOTYPE'
# imputed_data1$PHENOTYPE <- factor(imputed_data1$PHENOTYPE,levels = c(0,1),labels = c('Control','Case'))
# 
# 
# grep('categorical',label_select$ValueType[match(imputed_data1_id,label_select$FieldID)])
# 
# mod1 <- C5.0(PHENOTYPE ~ ., data = imputed_data1,
#              trials=100,
#              rules=F,
#              control = C5.0Control(winnow = T,minCases=20)) #model_res_all$Others$C5.0$model$bestTune$winnow
# mod1
# png(filename = 'C50tree.png',width = 2500,height = 800)
# plot(mod1, drop_terminal=T)
# dev.off()
# 
# aaa <- summary(mod1)
# mod1$output
# mode(aaa)
# 
# 
# 
# sapply(str_split(rownames(bbb)[bbb$Overall > 0],'[Xf\\.]'),function(x) setdiff(x,c('0','')))
# 
# # c5model = C5.0.default(x = traindata[,-ncol(testdata)], y = traindata[,ncol(testdata)],
# #                        trials = dt$bestTune$trials, rules = dt$bestTune$model == "rules",
# #                        control = C5.0Control(winnow = dt$bestTune$winnow))
# 
# length(which(predict(mod1,test_data_imputed)==test_data_imputed$PHENOTYPE))/nrow(test_data_imputed)
# length(which(predict(mod1,test_data_imputed,trials = 71)==test_data_imputed$PHENOTYPE))/nrow(test_data_imputed)
# 
# plot(mod1)
# 
# plot(mod1, subtree = 1)
# plot(mod1, trial = 33)
# 
# plot(mod1$boostResults$Size,mod1$boostResults$Percent)



# C5.0.graphviz(C5.0.model = mod2,filename = 'test2.txt')
# 'dot -Tpdf test2.txt -o test2.pdf'
# 
# 
# 
# 
# mod2 <- C5.0(Species ~ ., data = iris, trials = 10)
# plot(mod2) ## should be the same as above
# 
# ## plot first weighted tree
# plot(mod2, trial = 1)
# 
# 
# library("partykit")
# myTree1 <- C50:::as.party.C5.0(mod1)
# 
# myTree2 <- as.party(mod2$output)
# 
# plot.party(myTree2,terminal_panel = )
# print(myTree2)
# plot(myTree2, tp_args = list(text = TRUE))
# 
# ggplot(myTree2) + geom_party(aes(x = stat(x)))
# 
# 
# questionnaire <- as.simpleparty(myTree2)
# 
# # 打印问卷
# print(questionnaire)
# as.partynode(myTree2)
# 
# library(ggparty)
# ggparty(myTree2) +
#   geom_edge() +
#   geom_edge_label() +
#   geom_node_label(aes(label = splitvar),
#                   ids = "inner") +
#   geom_node_label(aes(label = info),
#                   ids = "terminal")
# 






# 
# 
# mod2 <- rpart.plot(PHENOTYPE ~ ., data = imputed_data,branch=1, fallen.leaves=T,cex=0.6)
# 
# 
# model.ID3 <- rpart( PHENOTYPE ~ ., data = imputed_data, 
#                     method="class", 
#                     parms=list(split="information")) #使用ID3算法时候, split = “information”
# length(which(predict(model.ID3,test_data_imputed,type ='class')==test_data_imputed$PHENOTYPE))/nrow(test_data_imputed)
# 
# printcp( model.ID3 )
# #绘制决策树图形
# rpart.plot( model.ID3, branch=1, type=1, fallen.leaves=T, cex=1, sub="决策树模型-ID3")
# library(rattle)
# fancyRpartPlot( model.ID3, 
#                 main = '', 
#                 sub = "zhengcf@ysu.edu.cn")
# 
# 
# data(ptitanic)
# model <- rpart(survived ~ ., data = ptitanic, cp = .02)
# rpart.plot(model)
# rpart.rules(model)


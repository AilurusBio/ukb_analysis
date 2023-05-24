require(xlsx)
require(stringr)
require(caret)
label_select_base_1 <- read.xlsx('./feature_info/label_selected20221228.xlsx',sheetIndex = 1) 
label_select_job_2 <- read.xlsx('./feature_info/label_selected20221228.xlsx',sheetIndex = 3) 
label_select_feed_3 <- read.xlsx('./feature_info/label_selected20221228.xlsx',sheetIndex = 4) 
label_select_life_4 <- read.xlsx('./feature_info/label_selected20221228.xlsx',sheetIndex = 5) 
label_select_smoke_5 <- read.xlsx('./feature_info/label_selected20221228.xlsx',sheetIndex = 6) 
label_select_blood_6 <- read.xlsx('./feature_info/label_selected20221228.xlsx',sheetIndex = 8) 

label_select_base_1 <- label_select_base_1[!label_select_base_1$ValueType %in% c('Date' ,'Info' ),]

load('S1_sample_id.RData')
load('S2_traintest_id.RData')
P_AD_id <- names(P_AD)
P_AD_id[grep('X',P_AD_id)] <- as.numeric(str_split(P_AD_id[grep('X',P_AD_id)],'[X\\.]',simplify = T)[,2])
P_AD_id[grep('f',P_AD_id)] <- as.numeric(str_split( grep('f',P_AD_id[grep('f',P_AD_id)],value = T),'[f\\.]',simplify = T)[,3])


#P_AD_id[which(P_AD_id %in% label_select_blood_6$FieldID)]
#table(P_AD_id[which(P_AD_id %in% label_select_blood_6$FieldID)])
P_AD_id[match(label_select_blood_6$FieldID,P_AD_id)]


select_feature <- na.omit(match(c(label_select_base_1$FieldID,
                                  label_select_job_2$FieldID,
                                  label_select_feed_3$FieldID,
                                  label_select_life_4$FieldID,
                                  label_select_smoke_5$FieldID,
                                  label_select_blood_6$FieldID),P_AD_id))

label_select <- list(Base=label_select_base_1[na.omit(match(P_AD_id[select_feature],label_select_base_1$FieldID)),c(1,5,6,7,8,9,10,17,18)],
                     Job=label_select_job_2[na.omit(match(P_AD_id[select_feature],label_select_job_2$FieldID)),c(1,5,6,7,8,9,10,17,18)],
                     Feed=label_select_feed_3[na.omit(match(P_AD_id[select_feature],label_select_feed_3$FieldID)),c(1,5,6,7,8,9,10,17,18)],
                     Life=label_select_life_4[na.omit(match(P_AD_id[select_feature],label_select_life_4$FieldID)),c(1,5,6,7,8,9,10,17,18)],
                     SmokingAndDrinking=label_select_smoke_5[na.omit(match(P_AD_id[select_feature],label_select_smoke_5$FieldID)),c(1,5,6,7,8,9,10,17,18)],
                     Blood=label_select_blood_6[na.omit(match(P_AD_id[select_feature],label_select_blood_6$FieldID)),c(1,5,6,7,8,9,10,17,18)])

label_select <- do.call('rbind',label_select)
head(label_select)
label_select$Source <- str_split(rownames(label_select),'\\.',simplify = T)[,1]
write.csv(label_select,file = 'S3_label_select.csv')
label_select <- label_select[-2,]
#################################################  other feature

select_feature <- match((label_select$FieldID[label_select$Source != "Blood"]),P_AD_id) 
P_AD$X34.0.0 <- P_AD$age

train_data <- P_AD[match(c(case_eid_trainK,control_eid_trainK),P_AD$eid),select_feature]
dim(train_data)
train_data <- train_data[,sapply(train_data, function(x) 1-length(which(is.na(x)))/length(x)) > 0.7]
train_data$PHENOTYPE <- factor(c(rep(1,length(case_eid_trainK)),rep(0,length(control_eid_trainK))))
dim(train_data)
rownames(train_data) <- c(case_eid_trainK,control_eid_trainK)

test_data <- P_AD[match(setdiff(c(case_eid,control_eid_11),
                                 c(case_eid_trainK,control_eid_trainK)),P_AD$eid),select_feature]
test_data$PHENOTYPE <- factor(c(rep(1,length(case_eid)-length(case_eid_trainK)),
                                rep(0,length(control_eid_11)-length(control_eid_trainK))))
test_data <- test_data[,colnames(train_data)]
rownames(test_data) <- setdiff(c(case_eid,control_eid_11),
                               c(case_eid_trainK,control_eid_trainK))
##### deal Categorical

FieldID_Categorical <- label_select$FieldID[label_select$Source != "Blood"][which( label_select$ValueType[label_select$Source != "Blood"] %in% c('Categorical','Categorical_complex','Categorical_repair'))]
Field_Categorical <- colnames(train_data)[sapply(colnames(train_data), function(x)
  sum(paste0('X',FieldID_Categorical,'.0.0')==x)+ 
    sum(paste0('f.',FieldID_Categorical,'.0.0')==x))==1]

train_data_other <- train_data
train_data_other[,Field_Categorical] <- lapply(train_data_other[,Field_Categorical], function(x) {
  x[x==-3] <- NA
  x#factor(x)
})
test_data_other <- test_data
test_data_other[,Field_Categorical] <- lapply(test_data_other[,Field_Categorical],  function(x) {
  x[x==-3] <- NA
  x#factor(x)
})

##################  blood feature
train_data_blood <- P_AD[match(rownames(train_data),P_AD$eid),
                         match((label_select$FieldID[label_select$Source == "Blood"]),P_AD_id) ]
train_data_blood <- train_data_blood[,setdiff(colnames(train_data_blood),
                                              c( 'X23000.0.0', 'X23001.0.0', 'X23002.0.0', 'X23010.0.0', 'X23011.0.0', 'X23012.0.0', 'X23013.0.0'))]

test_data_blood <- P_AD[match(rownames(test_data),P_AD$eid),
                         match((label_select$FieldID[label_select$Source == "Blood"]),P_AD_id) ]
test_data_blood <- test_data_blood[,setdiff(colnames(test_data_blood),
                                              c( 'X23000.0.0', 'X23001.0.0', 'X23002.0.0', 'X23010.0.0', 'X23011.0.0', 'X23012.0.0', 'X23013.0.0'))]

##################  test snp feature
load('S4_lm_res.RData')
#table(data$FID[as.numeric(rownames(lm_res$train_data))] == rownames(train_data))
#table(data$FID[as.numeric(rownames(lm_res$test_data))] == rownames(test_data))
train_data_snp <- lm_res$train_data[,-1]
test_data_snp <- lm_res$test_data[,-1]

################## test lm model 
# 
# formula <- PHENOTYPE ~ .
# 
# # Fit the model
# model <- glm(formula, data = train_data, family = "binomial")
# 
# # Make predictions on the test set
# predictions <- predict(model, train_data, type = "response")
# 
# # Convert the predictions to a binary class
# predictions_train <- ifelse( predict(model, train_data, type = "response") > 0.5, "1", "0")
# predictions_test <- ifelse( predict(model, test_data, type = "response") > 0.5, "1", "0")
# # Evaluate the model
# table(predictions_train, train_data$PHENOTYPE)
# length(which(predictions_train==train_data$PHENOTYPE))/length(predictions_train)
# 
# table(predictions_test, test_data$PHENOTYPE)

#################################

library(caret)
model_res_all <- list()
for (i in c('SNP', 'Others' ,'Others+blood', 'Others+blood+SNP')) {
  cat('--------------------\n')
  print(i)
  if (i=='SNP') {
    train_data <- cbind(PHENOTYPE=train_data_other$PHENOTYPE,train_data_snp)
    test_data <-  cbind(PHENOTYPE=test_data_other$PHENOTYPE,test_data_snp)
  }
  if(i=='Others'){
    train_data <- train_data_other
    test_data <- test_data_other
  }
  if(i=='Others+blood'){
    train_data <- cbind(train_data_other,train_data_blood)
    test_data <-  cbind(test_data_other,test_data_blood)
  }
  if(i=='Others+blood+SNP'){
    train_data <- cbind(train_data_other,train_data_blood,train_data_snp)
    test_data <-  cbind(test_data_other,test_data_blood,test_data_snp)
  }
  
  #####
  train_data$PHENOTYPE <- factor(train_data$PHENOTYPE,levels = c(1,0),labels = c('case','control'))
  test_data$PHENOTYPE <- factor(test_data$PHENOTYPE,levels = c(1,0),labels = c('case','control'))
  
  # Set up the control function for the model
  formula <- PHENOTYPE ~ .
  
  # Use the caret function to train a model using automl
  imputer <- preProcess(train_data, method = "medianImpute")
  imputed_data <- predict(imputer, train_data)
  test_data_imputed <- predict(imputer, test_data)
  
  
  model_res <- list()
  for (method in c('avNNet',"svmLinear", "svmRadial",'knn','ranger','gbm','bayesglm','C5.0')) {
    #method <- 'avNNet'
    print(method)
    if (method=='ranger') {
      set.seed(4321)
      ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE)
      model <- caret::train(formula, data = imputed_data, method = "ranger", trControl = ctrl,  importance = 'impurity')
    }else{
      set.seed(4321)
      model <- caret::train(formula,
                            data = imputed_data,
                            method = method, #
                            trControl = trainControl(method = "cv", number = 5,classProbs = TRUE))
    }

    # View the results
    
    predictions <- predict(model, test_data_imputed)
    model_res[[method]] <- list(predictions=predictions,
                                predictions_prob=predict(model, test_data_imputed ,type = "prob"),
                                confusionRes=confusionMatrix(predictions, test_data$PHENOTYPE),
                                model=model)
  }
  model_res_all[[i]] <- model_res
  print(sapply(model_res, function(x) x$confusionRes$byClass))

}

save(train_data_other,train_data_blood,train_data_snp,test_data_other,test_data_blood,test_data_snp,
     model_res_all,file = 'S5_model_res.RData')




# #################################
# library(caret)
# train_data <- cbind(train_data_other,train_data_blood)
# test_data <-  cbind(test_data_other,test_data_blood)
# # Set up the control function for the model
# formula <- PHENOTYPE ~ .
# 
# # Use the caret function to train a model using automl
# imputer <- preProcess(train_data, method = "medianImpute")
# imputed_data <- predict(imputer, train_data)
# test_data_imputed <- predict(imputer, test_data)
# 
# 
# model_res <- list()
# for (method in c('pcaNNet',"svmLinear", "svmRadial",'knn','ranger','gbm','bayesglm','C5.0')) {
#   #method <- 'knn'
#   print(method)
#   model <- caret::train(formula,
#                         data = imputed_data,
#                         method = method, #
#                         trControl = trainControl(method = "cv", number = 5),
#                         na.action=na.omit)
#   # View the results
#   predictions <- predict(model, test_data_imputed)
#   model_res[[method]] <- list(predictions=predictions,predictions_prob=predict(model, test_data_imputed ,type = "prob"),
#                               confusionRes=confusionMatrix(predictions, test_data$PHENOTYPE),
#                               model=model)
# }
# model_res_otherANDblood <- model_res
# sapply(model_res_otherANDblood, function(x) x$confusionRes$byClass)
# 
# #################################
# library(caret)
# train_data <- cbind(train_data_other,train_data_blood,train_data_snp)
# test_data <-  cbind(test_data_other,test_data_blood,test_data_snp)
# # Set up the control function for the model
# formula <- PHENOTYPE ~ .
# 
# # Use the caret function to train a model using automl
# imputer <- preProcess(train_data, method = "medianImpute")
# imputed_data <- predict(imputer, train_data)
# test_data_imputed <- predict(imputer, test_data)
# 
# 
# model_res <- list()
# for (method in c('pcaNNet',"svmLinear", "svmRadial",'knn','ranger','gbm','bayesglm','C5.0')) {
#   #method <- 'knn'
#   print(method)
#   model <- caret::train(formula,
#                         data = imputed_data,
#                         method = method, #
#                         trControl = trainControl(method = "cv", number = 5),
#                         na.action=na.omit)
#   # View the results
#   predictions <- predict(model, test_data_imputed)
#   model_res[[method]] <- list(predictions=predictions,predictions_prob=predict(model, test_data_imputed ,type = "prob"),
#                               confusionRes=confusionMatrix(predictions, test_data$PHENOTYPE),
#                               model=model)
# }
# model_res_otherANDbloodANDsnp <- model_res
# sapply(model_res_otherANDbloodANDsnp, function(x) x$confusionRes$byClass)
# 
# ################
# library(caret)
# train_data <- cbind(train_data_other,train_data_blood,train_data_snp)
# test_data <-  cbind(test_data_other,test_data_blood,test_data_snp)
# # Set up the control function for the model
# formula <- PHENOTYPE ~ .
# 
# # Use the caret function to train a model using automl
# imputer <- preProcess(train_data, method = "medianImpute")
# imputed_data <- predict(imputer, train_data)
# test_data_imputed <- predict(imputer, test_data)
# 
# 
# model_res <- list()
# for (method in c('pcaNNet',"svmLinear", "svmRadial",'knn','ranger','gbm','bayesglm','C5.0')) {
#   #method <- 'knn'
#   print(method)
#   model <- caret::train(formula,
#                         data = imputed_data,
#                         method = method, #
#                         trControl = trainControl(method = "cv", number = 5),
#                         na.action=na.omit)
#   # View the results
#   predictions <- predict(model, test_data_imputed)
#   model_res[[method]] <- list(predictions=predictions,predictions_prob=predict(model, test_data_imputed ,type = "prob"),
#                               confusionRes=confusionMatrix(predictions, test_data$PHENOTYPE),
#                               model=model)
# }
# model_res_otherANDbloodANDsnp <- model_res
# sapply(model_res_otherANDbloodANDsnp, function(x) x$confusionRes$byClass)
# 
# 
# ################
# library(caret)
# train_data <- cbind(PHENOTYPE=train_data_other$PHENOTYPE,train_data_snp)
# test_data <-  cbind(PHENOTYPE=test_data_other$PHENOTYPE,test_data_snp)
# # Set up the control function for the model
# formula <- PHENOTYPE ~ .
# 
# # Use the caret function to train a model using automl
# imputer <- preProcess(train_data, method = "medianImpute")
# imputed_data <- predict(imputer, train_data)
# test_data_imputed <- predict(imputer, test_data)
# 
# 
# model_res <- list()
# for (method in c('pcaNNet',"svmLinear", "svmRadial",'knn','ranger','gbm','bayesglm','C5.0')) {
#   #method <- 'knn'
#   print(method)
#   levels(imputed_data$PHENOTYPE)
#   imputed_data$PHENOTYPE <- factor(imputed_data$PHENOTYPE,labels = c('control','case'))
#   make.names(levels(imputed_data$PHENOTYPE))
#   model <- caret::train(formula,
#                         data = imputed_data,
#                         method = method, #
#                         trControl = trainControl(method = "cv", number = 5,classProbs = TRUE),
#                         na.action=na.omit
# )
#   predict(model, test_data_imputed ,type = "prob")
#   # View the results
#   predictions <- predict(model, test_data_imputed)
#   model_res[[method]] <- list(predictions=predictions,
#                               predictions_prob=predict(model, test_data_imputed ,type = "prob"),
#                               confusionRes=confusionMatrix(predictions, test_data$PHENOTYPE),
#                               model=model)
# }
# model_res_snp <- model_res
# sapply(model_res_snp, function(x) x$confusionRes$byClass)
# # Evaluate the model
# 
# train_data <- cbind(train_data_other,train_data_blood,train_data_snp)
# test_data <-  cbind(test_data_other,test_data_blood,test_data_snp)



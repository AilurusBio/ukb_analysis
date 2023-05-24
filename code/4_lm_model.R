getwd()

data <- c()
for(file in grep('model_data.*.raw',dir('gwas/',full.names = T),value = T)){
  data_tmp <- read.table(file,header=T,stringsAsFactors = F)
  if(length(data)==0){
    data <- data_tmp
  }else{
    data <- cbind(data,data_tmp[,-c(1:6),drop=F])
  }
  
  dim(data)
  print(file)
}
data[,-c(1:6)][is.na(data[,-c(1:6)])] <- 0
data[,-c(1:6)][(data[,-c(1:6)])>1] <- 1
dim(data)
load('S2_traintest_id.RData')
# case_eid_trainK,control_eid_trainK,case_eid,control_eid,control_eid_11

train_data <- data[match(c(case_eid_trainK,control_eid_trainK),data$FID),-c(1:5)]
train_data$PHENOTYPE <- factor(train_data$PHENOTYPE-1)

test_data <- data[match(setdiff(c(case_eid,control_eid_11),
                                c(case_eid_trainK,control_eid_trainK)),data$FID),-c(1:5)]
test_data$PHENOTYPE <- factor(test_data$PHENOTYPE-1)

formula <- PHENOTYPE ~ .

# Fit the model
model_lm <- glm(formula, data = train_data, family = "binomial")

# Make predictions on the test set
predictions <- predict(model_lm, data, type = "response")

# Convert the predictions to a binary class
predictions_train <- ifelse( predict(model_lm, train_data, type = "response") > 0.5, "1", "0")
predictions_test <- ifelse( predict(model_lm, test_data, type = "response") > 0.5, "1", "0")

# Evaluate the model
table(predictions_train, train_data$PHENOTYPE)
table(predictions_test, test_data$PHENOTYPE)

lm_res <- list(train_data=train_data,
               test_data=test_data,
               model_lm=model_lm,
               coef=model_lm$coefficients,
               predict_train=predict(model_lm, train_data, type = "response") ,
               predict_test=predict(model_lm, test_data, type = "response") )

save(data,lm_res,file = 'S4_lm_res.RData')
#############################################
# library(caret)
# 
# model <- caret::train(formula,
#                       data = train_data,
#                       method = c("svmLinear", "svmRadial", "glm",'multinom','ranger','gbm'),
#                       trControl = trainControl(method = "cv", number = 10),
#                       preProcess = c("center", "scale"))
# 
# # Make predictions on the test set
# predictions <- predict(model, test_data)
# 
# # Evaluate the model
# confusionMatrix(predictions, test_data$PHENOTYPE)







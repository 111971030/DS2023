if(!require('tidyverse')) {
    install.packages('tidyverse')
    library('tidyverse')
}
# Load the feature.R file
source("feature.R")
data <- read_csv("Titanic/train.csv")
res <- preprocess_data(data)

#=========== 訓練模型 =========== 
# Logistic regression
library(caret)

# 設定5-fold
lr_ctrl <- trainControl(method="cv", 
                        number=5, 
                        savePredictions="all",
                        classProbs=TRUE)

logistic_model <- train(Survived ~ ., data = res$train, method="glm", family = "binomial", trControl = lr_ctrl)

# 預測訓練集
train_pred <- predict(logistic_model, newdata = res$train)
lr_train_predicted <- predict(logistic_model, newdata = res$train)
# 概率轉換為類別
lr_train_predicted_class <- ifelse(lr_train_predicted > 0.5, 1, 0)
# 準確率
lr_train_accuracy <- sum(lr_train_predicted_class == res$train$Survived) / nrow(res$train)
cat("Logistic Regression training data Accuracy:",lr_train_accuracy,"\n")
# Confusion matrix
lr_train_confusion_matrix <- table(Actual = res$train$Survived, Predicted = lr_train_predicted_class)
cat("Logistic Regression training data Confusion Matrix:\n")
print(lr_train_confusion_matrix)

# 預測驗證集
lr_validate_predicted <- predict(logistic_model, newdata = res$validate)
# 概率轉換為類別
lr_validate_predicted_class <- ifelse(lr_validate_predicted > 0.5, 1, 0)
# 準確率
lr_validate_accuracy <- sum(lr_validate_predicted_class == res$validate$Survived) / nrow(res$validate)
cat("Logistic Regression validation data Accuracy:",lr_validate_accuracy,"\n")
# Confusion matrix
lr_validate_confusion_matrix <- table(Actual = res$validate$Survived, Predicted = lr_validate_predicted_class)
cat("Logistic Regression validation data Confusion Matrix:\n")
print(lr_validate_confusion_matrix)

# 預測測試集
lr_test_predicted <- predict(logistic_model, newdata = res$test)
# 概率轉換為類別
lr_test_predicted_class <- ifelse(lr_test_predicted > 0.5, 1, 0)
# 準確率
lr_test_accuracy <- sum(lr_test_predicted_class == res$test$Survived) / nrow(res$test)
cat("Logistic Regression test data Accuracy:",lr_test_accuracy,"\n")
# Confusion matrix
lr_test_confusion_matrix <- table(Actual = res$test$Survived, Predicted = lr_test_predicted_class)
cat("Logistic Regression test data Confusion Matrix:\n")
print(lr_test_confusion_matrix)

#========================
# K nearest neighbors
library(caret)

# 設定5-fold
knn_ctrl <- trainControl(method="cv", 
                         number=5, 
                         savePredictions="all",
                         classProbs=TRUE)

#設定KNN的K值列表
knn_list <- c(3, 5, 7, 9, 11)

#knn_result_list <- list()
knn_result_list <- vector("list", length(knn_list))

for (i in 1:length(knn_list)) {
  cat("當K = ", knn_list[i], "\n")
  
  knn_model <- train(
    Survived ~ .,  # 使用所有特徵變數進行預測
    data = res$train,
    method = "knn",
    trControl = knn_ctrl,
    preProcess = c("center", "scale"),
    tuneLength = knn_list[i]  # 设置调参范围，例如k的取值范围
  )
  
  # 預測訓練集
  train_predicted <- predict(knn_model, newdata = res$train)
  # 概率轉換為類別
  train_predicted_class <- ifelse(train_predicted > 0.5, 1, 0)
  # 準確率
  train_accuracy <- sum(train_predicted_class == res$train$Survived) / nrow(res$train)
  # Confusion matrix
  train_confusion_matrix <- table(Actual = res$train$Survived, Predicted = train_predicted_class)
  
  # 預測驗證集
  validate_predicted <- predict(knn_model, newdata = res$validate)
  # 概率轉換為類別
  validate_predicted_class <- ifelse(validate_predicted > 0.5, 1, 0)
  # 準確率
  validate_accuracy <- sum(validate_predicted_class == res$validate$Survived) / nrow(res$validate)
  # Confusion matrix
  validate_confusion_matrix <- table(Actual = res$validate$Survived, Predicted = validate_predicted_class)
  
  # 預測測試集
  test_predicted <- predict(knn_model, newdata = res$test)
  # 概率轉換為類別
  test_predicted_class <- ifelse(test_predicted > 0.5, 1, 0)
  # 準確率
  test_accuracy <- sum(test_predicted_class == res$test$Survived) / nrow(res$test)
  # Confusion matrix
  test_confusion_matrix <- table(Actual = res$test$Survived, Predicted = test_predicted_class)
  
  # 記錄預測訓練及的準確值
  knn_result_list[[i]] <- list(k = knn_list[i],
                               train_accuracy = train_accuracy,
                               train_cf_table = train_confusion_matrix,
                               validate_accuracy = validate_accuracy,
                               validate_cf_table = validate_confusion_matrix,
                               test_accuracy = test_accuracy,
                               test_cf_table = test_confusion_matrix)
}

# 找到KNN列表中的最高準確率和其索引
max_index <- which.max(sapply(knn_result_list, function(x) x$train_accuracy))
best_k <- knn_result_list[[max_index]]$k
cat("KNN with k=", best_k," in k list",knn_list, " got best training accuracy.","\n")
cat("KNN training data Accuracy:",knn_result_list[[max_index]]$train_accuracy,"\n")
cat("KNN training data Confusion Matrix:\n")
print(knn_result_list[[max_index]]$train_cf_table)

cat("KNN validation data Accuracy:",knn_result_list[[max_index]]$validate_accuracy,"\n")
cat("KNN validation data Confusion Matrix:\n")
print(knn_result_list[[max_index]]$validate_cf_table)

cat("KNN test data Accuracy:",knn_result_list[[max_index]]$test_accuracy,"\n")
cat("KNN test data Confusion Matrix:\n")
print(knn_result_list[[max_index]]$test_cf_table)




# 安裝所需套件e1071
 if(!require('e1071')) {
   install.packages('e1071')
   library('e1071')
 }

# SVC_Linear_Model、SVC_RBF_Model
TrainingModel_Sophia <- function(model_name, data)
{
  if(model_name == "linear")
  {
    kernel = "linear"
    #message("建立 SVC Linear 模型並訓練模型")
  }
  else if (model_name == "RBF")
  {
    kernel = "radial"
    #message("建立 SVC RBF 模型並訓練模型")
  }
  # 目標因子
  data$train$Survived <- as.factor(data$train$Survived)
  # 1. 模型建模 function(SVC_Linear、SVC_RBF)
  model <- svm(Survived ~ ., 
               data = data$train, 
               kernel = kernel,
               scale = FALSE
  )
  # 2. 預測訓練資料集：
  train_predictions <- predict(model, newdata = data$train)
  # 2.1. 準確率(Accuracy)
  train_accuracy <- sum(train_predictions == data$train$Survived) / nrow(data$train)
  # 2.2. 混淆矩陣(Confusion matrix)
  train_cm = table(train_predictions,data$train$Survived)

  # 3. 預測測試資料集：
  test_predictions <- predict(model, newdata = data$test)
  # 3.1. 準確率(Accuracy)
  test_accuracy <- sum(test_predictions == data$test$Survived) / nrow(data$test)
  # 3.2. 混淆矩陣(Confusion matrix)
  test_cm = table(test_predictions,data$test$Survived)

  # 4. 預測驗證資料集：
  validate_predictions <- predict(model, newdata = data$validate)
  # 4.1. 準確率(Accuracy)
  validate_accuracy <- sum(validate_predictions == data$validate$Survived) / nrow(data$validate)
  # 4.2. 混淆矩陣(Confusion matrix)
  validate_cm = table(validate_predictions,data$validate$Survived)

  # 回傳
  return_list = list(model = model, 
                     train_accuracy = train_accuracy, train_cm = train_cm,
                     test_accuracy = test_accuracy, test_cm = test_cm,
                     validate_accuracy = validate_accuracy, validate_cm = validate_cm
                    )
  return(return_list)
}

## 印出SVC Linear結果>>>>>>>>>>>>>
message("======= SVC Linear Model =======")
SVC_Linear_Model = TrainingModel_Sophia("linear",res)
# 預測訓練資料集(train)
cat("使用train data進行預測的準確率：", SVC_Linear_Model$train_accuracy)
print(SVC_Linear_Model$train_cm)
# 預測測試資料集(test)
cat("使用test data進行預測的準確率：", SVC_Linear_Model$test_accuracy)
print(SVC_Linear_Model$test_cm)
# 預測驗證資料集(validate)
cat("使用validate data進行預測的準確率：", SVC_Linear_Model$validate_accuracy)
print(SVC_Linear_Model$validate_cm)


## 印出SVC RBF結果>>>>>>>>>>>>>
message("======= SVC RBF Model =======")
SVC_RBF_Model = TrainingModel_Sophia("RBF",res)
# 預測訓練資料集(train)
cat("使用train data進行預測的準確率：", SVC_RBF_Model$train_accuracy)
print(SVC_RBF_Model$train_cm)
# 預測測試資料集(test)
cat("使用test data進行預測的準確率：", SVC_RBF_Model$test_accuracy)
print(SVC_RBF_Model$test_cm)
# 預測驗證資料集(validate)
cat("使用validate data進行預測的準確率：", SVC_RBF_Model$validate_accuracy)
print(SVC_RBF_Model$validate_cm)



# Use GaussianNB
if (!require('e1071')) {
  install.packages('e1071')
  library('e1071')
}

ga_model <- naiveBayes(Survived ~ ., data = res$train)

# training accuracy
ga_pred_train <- predict(ga_model, newdata = res$train)
ga_accuracy_train <- mean(ga_pred_train == res$train$Survived)
cat("GaussianNB Training accuracy:", ga_accuracy_train, "\n")

# test accuracy
ga_pred_test <- predict(ga_model, newdata = res$test)
ga_accuracy_test <- mean(ga_pred_test == res$test$Survived)
cat("GaussianNB Test Accuracy:", ga_accuracy_test, "\n")

# validation accuracy
ga_pred_validate <- predict(ga_model, newdata = res$validate)
ga_accuracy_validate <- mean(ga_pred_validate == res$validate$Survived)
cat("GaussianNB Validation Accuracy:", ga_accuracy_validate, "\n")

# confusion matrix
ga_confusion_matrix <- table(Actual = res$test$Survived, Predicted = ga_pred_test)
cat("GaussianNB Confusion Matrix:\n")
print(ga_confusion_matrix)

# Use Dicision Tree
library(rpart)

dt_model <- rpart(Survived ~., data = res$train, method = "class")

# training accuracy
dt_pred_train <- predict(dt_model, res$train, type = "class")
dt_accuracy_train <- mean(dt_pred_train == res$train$Survived)
cat("Dicision Tree Training accuracy:", dt_accuracy_train, "\n")

# test accuracy
dt_pred_test <- predict(dt_model, res$test, type = "class")
dt_accuracy_test <- mean(dt_pred_test == res$test$Survived)
cat("Dicision Tree Test Accuracy:", dt_accuracy_test, "\n")

# validation accuracy
dt_pred_validate <- predict(dt_model, res$validate, type = "class")
dt_accuracy_validate <- mean(dt_pred_validate == res$validate$Survived)
cat("Decision Tree Validation Accuracy:", dt_accuracy_validate, "\n")

# confusion matrix
dt_confusion_matrix <- table(Actual = res$test$Survived, Predicted = dt_pred_test)
cat("Dicision Tree Confusion Matrix:\n")
print(dt_confusion_matrix)

# Use the RandomForestClassifier
library(randomForest)

rf_model <- randomForest(factor(Survived) ~ ., data = res$train, ntree = 500)

# training accuracy
rf_pred_train <- predict(rf_model, newdata = res$train)
rf_accuracy_train <- mean(rf_pred_train == res$train$Survived)
cat("Random Forest Training Accuracy:", rf_accuracy_train, "\n")

# test accuracy
rf_pred_test <- predict(rf_model, newdata = res$test)
rf_accuracy_test <- mean(rf_pred_test == res$test$Survived)
cat("Random Forest Test Accuracy:", rf_accuracy_test, "\n")

# validation accuracy
rf_pred_validate <- predict(rf_model, newdata = res$validate)
rf_accuracy_validate <- mean(rf_pred_validate == res$validate$Survived)
cat("Random Forest Validation Accuracy:", rf_accuracy_validate, "\n")

# confusion matrix
rf_confusion_matrix <- table(Actual = res$test$Survived, Predicted = rf_pred_test)
cat("Random Forest Confusion Matrix:\n")
print(rf_confusion_matrix)


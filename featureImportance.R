library(tidyverse)
library(caret)
library('e1071')
library(rpart)

# Load the feature.R file
source("feature.R")
train_data <- read_csv("Titanic/train.csv")
preprocess_train_data <- preprocess_data(train_data)

# test_data <- read_csv("Titanic/test.csv")
# preprocess_test_data <- preprocess_data(test_data)


#=========== 訓練模型 =========== 
TrainingModel <- function(model_name, train_data)
{
  # Set up the trainControl object for 5-fold cross-validation
  ctrl <- trainControl(method = "cv", number = 5, savePredictions = "all", classProbs = TRUE)
  
  Training_model <- NULL
  
  if(model_name == "Logistic regression"){
    Training_model <- train(Survived ~ ., data = train_data, method = "glm", trControl = ctrl, family = "binomial")
  } else if(model_name == "K nearest neighbors"){
    Training_model <- train(Survived ~ ., data = train_data, method = "knn", trControl = ctrl, preProcess = c("center", "scale"),tuneLength = 3)
  } else if(model_name == "SVC Linear"){
    Training_model <- train(Survived ~ ., data = train_data, kernel = "svmLinear", trControl = ctrl, scale = FALSE)
  } else if(model_name == "SVC RBF"){
    Training_model <- train(Survived ~ ., data = train_data, kernel = "svmRadial", trControl = ctrl, scale = FALSE)
  } else if(model_name == "Gaussian Naive Bayes"){
    Training_model <- train(Survived ~ ., data = train_data, kernel = "naiveBayes", trControl = ctrl)
  } else if(model_name == "Decision Tree"){
    Training_model <- train(Survived~., data = train_data, method = 'rpart', trControl = ctrl)
  } else if(model_name == "Random Forest Classifier"){
    Training_model <- train(Survived~., data = train_data, method = 'rf', trControl = ctrl)
  }
  
  # 預測訓練集
  res_predictions <- predict(Training_model, newdata = train_data)
  res_accuracy <- sum(res_predictions == train_data$Survived) / nrow(train_data)
  res_cm <- table(res_predictions,train_data$Survived)
  
  ## 印出結果
  message(sprintf("======= %s Model =======", model_name))
  cat("使用training data進行預測的準確率：", res_accuracy , '\n')
  print(res_cm)

  # 特徵重要性排序
  if (model_name == "K nearest neighbors") {
    cat("該模型不支援特徵重要性排序\n")
  } else {
    var_imp <- varImp(Training_model)$importance
    cat("\n")
    cat("特徵重要性排序：\n")
    print(var_imp)
    cat("\n")
  }
}

TrainingModel("Logistic regression",preprocess_train_data)
TrainingModel("K nearest neighbors",preprocess_train_data)
TrainingModel("SVC Linear",preprocess_train_data)
TrainingModel("SVC RBF",preprocess_train_data)
TrainingModel("Gaussian Naive Bayes",preprocess_train_data)
TrainingModel("Decision Tree",preprocess_train_data)
TrainingModel("Random Forest Classifier",preprocess_train_data)

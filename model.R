library(tidyverse)
library(caret)
library('e1071')
library(rpart)
library(ggplot2)
library(dplyr)
library(pROC)

# Load the feature.R file
source("feature.R")
train_data <- read_csv("Titanic/train.csv")
preprocess_train_data <- preprocess_data(train_data, "training")

test_data <- read_csv("Titanic/test.csv")
preprocess_test_data <- preprocess_data(test_data, "test")

#=========== 取得預測結果 =========== 
GetPredInfo <- function(Training_model, train_data){
  # Predict on the training dataset
  res_predictions <- predict(Training_model, newdata = train_data)
  res_accuracy <- sum(res_predictions == train_data$Survived) / nrow(train_data)
  # Compute confusion metrics
  res_confusion <- confusionMatrix(res_predictions, train_data$Survived)$table
  # Calculate F1 score, precision, and recall
  tp <- res_confusion[2, 2]
  fp <- res_confusion[1, 2]
  fn <- res_confusion[2, 1]

  res_precision <- tp / (tp + fp)
  res_recall <- tp / (tp + fn)
  res_f1_score <- (2 * res_precision * res_recall) / (res_precision + res_recall)
  
  # Compute ROC AUC value
  res_predictions_prob <- predict(Training_model, newdata = train_data, type = "prob")[, 2]
  roc_obj <- roc(train_data$Survived, res_predictions_prob, quiet=TRUE)
  res_auc <- roc_obj$auc
  
  predInfo <- c(accuracy = round(res_accuracy, digits = 4) , F1 = round(res_f1_score, digits = 4), precision = round(res_precision, digits = 4), recall = round(res_recall, digits = 4), auc = round(res_auc, digits = 4))
  
  return(predInfo)
}

#=========== 訓練模型 =========== 
TrainingModel <- function(model_name, splited_data)
{
    # model_name <- "Logistic regression"
    train_data <- splited_data$train
    valid_data <- splited_data$valid
    test_data <- splited_data$test
    # Set up the trainControl object for 5-fold cross-validation
    ctrl <- trainControl(method = "cv", number = 5, savePredictions = "all", classProbs = TRUE)
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
    
    trainingPred <- GetPredInfo(Training_model, train_data)
    validPred <- GetPredInfo(Training_model, valid_data)
    testPred <- GetPredInfo(Training_model, test_data)
    
    dfPred <- rbind(trainingPred,validPred,testPred)
    
    # Print the results
    message(sprintf("======= %s Model =======", model_name))
    rownames(dfPred) <- c('Training data', 'Vaild data', 'Test data')
    colnames(dfPred) <- c('Accuracy', 'F1 Score', 'Precision', 'Recall', 'AUC')
    
    dfPred <- as.table(dfPred)
    print(dfPred)

    return(Training_model)
}

model_list <- c("Logistic regression","K nearest neighbors","SVC Linear","SVC RBF","Gaussian Naive Bayes","Decision Tree","Random Forest Classifier")
# model_list <- c("Logistic regression")
for (model_name in model_list) {
  #固定random資料
  set.seed(333)
  # 將現有資料 切分 80 % 作為訓練資料集 10 % 為 測試資料集 10 % 為 驗證資料集
  spec = c(train = .8, test = .1, valid = .1)
  
  data_train_sample = sample(cut(
    seq(nrow(preprocess_train_data)), 
    nrow(preprocess_train_data)*cumsum(c(0,spec)),
    labels = names(spec)
  ))
  
  splited_data = split(preprocess_train_data, data_train_sample)
  
  
  # Train the model
  Training_model <- TrainingModel(model_name, splited_data)
  
  # Generate predictions
  res_predictions <- predict(Training_model, newdata = preprocess_test_data)
  
  # Create predictions data frame
  dimensions <- dim(preprocess_test_data)
  ids <- 0:(dimensions[1] - 1)
  predictions <- data.frame(Id = ids, Probability = res_predictions)

  # Define output file path
  output_file <- paste("predictions/", model_name, " prediction.csv", sep = "")
  out_f_path <- dirname(output_file)

  # Create output directory if it doesn't exist
  if (!dir.exists(out_f_path)) {
    dir.create(out_f_path, recursive = TRUE)
  }

  print(sprintf("Write predictions to CSV file. Path : %s ", output_file))
  write.csv(predictions, file = output_file, row.names = FALSE)
}



library(tidyverse)
library(caret)
library('e1071')
library(rpart)

# Load the feature.R file
source("feature.R")
train_data <- read_csv("Titanic/train.csv")
preprocess_train_data <- preprocess_data(train_data)

test_data <- read_csv("Titanic/test.csv")
test_correct_data <- read_csv("Titanic/gender_submission.csv")
test_merged_data <- merge(test_data,test_correct_data,by=c('PassengerId'))

preprocess_test_data <- preprocess_data(test_merged_data)


#=========== 訓練模型 =========== 
TrainingModel <- function(model_name, train_data)
{
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

    # 預測訓練集
    res_predictions <- predict(Training_model, newdata = train_data)
    res_accuracy <- sum(res_predictions == train_data$Survived) / nrow(train_data)
    res_cm <- table(res_predictions,train_data$Survived)
    
    ## Print the results果
    message(sprintf("======= %s Model =======", model_name))
    cat("使用training data進行預測的準確率：", res_accuracy , '\n')
    print(res_cm)
    cat('\n')
    return(Training_model)
}

# model_list <- c("Logistic regression","K nearest neighbors","SVC Linear","SVC RBF","Gaussian Naive Bayes","Decision Tree","Random Forest Classifier")
model_list <- c("Logistic regression")
for (model_name in model_list) {
  # Train the model
  Training_model <- TrainingModel(model_name, preprocess_train_data)
  
  # Predict the outcomes for the test data
  res_predictions <- predict(Training_model, newdata = preprocess_test_data)

  # Calculate the accuracy of the predictions
  res_accuracy <- sum(res_predictions == preprocess_test_data$Survived) / nrow(preprocess_test_data)
  
  # Calculate the confusion matrix
  res_cm <- table(res_predictions,preprocess_test_data$Survived)
  
  ## Print the results
  cat("使用test data進行預測的準確率：", res_accuracy , '\n')
  print(res_cm)
  
  output_file <- "pred_"+model_name+".csv"
  predictions <- data.frame(Id = ids, Probability = rf_pred_test[, 2])
  # get output file path
  out_f_path <- dirname(output_file)
  if (!dir.exists(output_file)){
    dir.create(out_f_path, recursive = TRUE)
  }
  write.csv(predictions, file = output_file, row.names = FALSE)
}



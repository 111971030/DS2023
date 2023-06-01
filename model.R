library(tidyverse)
library(caret)
library('e1071')
library(rpart)
library(ggplot2)
library(dplyr)
# library('yardstick')
# Load the feature.R file
source("feature.R")
train_data <- read_csv("Titanic/train.csv")
preprocess_train_data <- preprocess_data(train_data, "training")

test_data <- read_csv("Titanic/test.csv")
preprocess_test_data <- preprocess_data(test_data, "test")


#=========== 訓練模型 =========== 
TrainingModel <- function(model_name, train_data)
{
    model_name <- "Logistic regression"
    train_data <- preprocess_train_data
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

    # Predict on the training set
    res_predictions <- predict(Training_model, newdata = train_data)
    res_accuracy <- sum(res_predictions == train_data$Survived) / nrow(train_data)
    res_cm <- table(res_predictions, train_data$Survived)
    
    # Compute additional metrics
    res_confusion <- confusionMatrix(res_predictions, train_data$Survived)
    res_f1 <- res_confusion$byClass["F1"]
    res_precision <- res_confusion$byClass["Pos Pred Value"]
    res_recall <- res_confusion$byClass["Sensitivity"]

    # Compute ROC AUC value
    res_predictions_prob <- predict(Training_model, newdata = train_data, type = "prob")[, 2]
    roc_obj <- roc(train_data$Survived, res_predictions_prob)
    res_auc <- roc_obj$auc
    
    # Plot importance list 
    importance <- varImp(Training_model)$importance
    importance <- importance[order(importance$Overall, decreasing = TRUE), , drop = FALSE]
    importance$Feature <- rownames(importance)
    # Rename the "Sex_factors" feature to "Sex"
    # Rename the "Embarked_factors" feature to "Embarked"
    importance$Feature <- ifelse(importance$Feature == "Sex_factors", "Sex", importance$Feature)
    importance$Feature <- ifelse(importance$Feature == "Embarked_factors", "Embarked", importance$Feature)
    importance_df <- as.data.frame(importance)
    importance_df$Feature <- factor(importance_df$Feature, levels = importance_df$Feature)
    
    
    ggplot(importance_df, aes(x = Feature, y = Overall)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = paste(model_name, " Feature Importance", sep = ""), x = "Feature", y = "Importance") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
    # Print the results
    message(sprintf("======= %s Model =======", model_name))
    cat("Accuracy :", res_accuracy, "\n")
    cat("F1 Score :", res_f1, "\n")
    cat("Precision:", res_precision, "\n")
    cat("Recall   :", res_recall, "\n")
    cat("ROC AUC  :", res_auc, "\n")
    # Plot confusion matrix
    plot_cm <- ggplot(as.data.frame(res_cm), aes(res_predictions, Var2, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = Freq), color = "black", size = 4) +
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(title = paste(model_name, " Confusion Matrix", sep = ""), x = "Prediction", y = "Accuracy")
    print(plot_cm)
    cat("\n")
    # Plot ROC curve
    par(pty="s")
    plot(roc_obj, main = paste(model_name, " ROC Curve", sep = ""), col = "blue", legacy.axes = TRUE, print.auc = TRUE)
    
    return(Training_model)
}

# model_list <- c("Logistic regression","K nearest neighbors","SVC Linear","SVC RBF","Gaussian Naive Bayes","Decision Tree","Random Forest Classifier")
model_list <- c("Logistic regression")
for (model_name in model_list) {
  # Train the model
  Training_model <- TrainingModel(model_name, preprocess_train_data)
  
  # Generate predictions
  res_predictions <- predict(Training_model, newdata = preprocess_test_data)
  
  # Create predictions data frame
  dimensions <- dim(preprocess_test_data)
  ids <- 0:(dimensions[1] - 1)
  predictions <- data.frame(Id = ids, Probability = res_predictions)

  # Define output file path
  output_file <- paste("predictions/pred_", model_name, ".csv", sep = "")
  out_f_path <- dirname(output_file)

  # Create output directory if it doesn't exist
  if (!dir.exists(out_f_path)) {
    dir.create(out_f_path, recursive = TRUE)
  }

  # Write predictions to CSV file
  write.csv(predictions, file = output_file, row.names = FALSE)
}



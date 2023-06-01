library(shiny)
library(tidyverse)
library(caret)
library(e1071)
library(rpart)
library(randomForest)
Sys.setlocale("LC_ALL", "en_US.UTF-8")
# Load the feature.R file
source("feature.R")
train_data <- read_csv("train.csv")
preprocess_train_data <- preprocess_data(train_data)

# UI
ui <- fluidPage(
  titlePanel("Modeling"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("model_name", "Select model", choices = c("Logistic regression", "K nearest neighbors", "SVC Linear", "SVC RBF", "Gaussian Naive Bayes", "Decision Tree", "Random Forest Classifier")),
      actionButton("runButton", "Run")
    ),
    
    mainPanel(
      h4("Training Accuracy"),
      verbatimTextOutput("trainingAccuracyOutput"),
      h4("Confusion Matrix"),
      plotOutput("confusionMatrixOutput"),
      h4("Feature Importance"),
      verbatimTextOutput("featureImportanceError"),
      plotOutput("featureImportanceOutput")
    )
  )
)

# Server
server <- function(input, output) {
  
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
    
    if (model_name == "K nearest neighbors") {
      importance_df <- NULL
    } else {
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
    }
    
    return(list(accuracy = res_accuracy, cm = res_cm, var_imp = importance_df))
  }
  
  
  results <- reactiveValues(accuracy = NULL, cm = NULL, var_imp = NULL)
  
  observeEvent(input$runButton, {
    model_name <- input$model_name
    model_results <- TrainingModel(model_name, preprocess_train_data)
    results$accuracy <- model_results$accuracy
    results$cm <- model_results$cm
    results$var_imp <- model_results$var_imp
  })
  
  output$trainingAccuracyOutput <- renderText({
    if (!is.null(results$accuracy)) {
      paste(results$accuracy)
    } else {
      " "
    }
  })
  
  output$confusionMatrixOutput <- renderPlot({
    if (!is.null(results$cm)) {
      ggplot(as.data.frame(res_cm), aes(res_predictions, Var2, fill = Freq)) +
        geom_tile() +
        geom_text(aes(label = Freq), color = "black", size = 4) +
        scale_fill_gradient(low = "white", high = "steelblue") +
        labs(title = paste(input$model_name, " Confusion Matrix", sep = ""), x = "Prediction", y = "Accuracy")
    } else {
      cat(" ")
    }
  })

  output$featureImportanceOutput <- renderPlot({
    if (!is.null(results$var_imp)) {
      ggplot(results$var_imp, aes(x = Feature, y = Overall)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(title = paste(input$model_name, " Feature Importance", sep = ""), x = "Feature", y = "Importance") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    } else {
      if (input$model_name == "K nearest neighbors") {
        cat("該模型不支援特徵重要性排序\n")
      } else {
        cat(" ")
      }
    }
  })
  
  output$featureImportanceError <- renderText({
    if (is.null(results$var_imp)) {
      cat("該模型不支援特徵重要性排序\n")
    }
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


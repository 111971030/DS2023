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
      verbatimTextOutput("confusionMatrixOutput"),
      h4("Feature Importance"),
      verbatimTextOutput("featureImportanceOutput")
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
      var_imp <- NULL
    } else {
      var_imp <- varImp(Training_model)$importance
    }
    
    return(list(accuracy = res_accuracy, cm = res_cm, var_imp = var_imp))
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
  
  output$confusionMatrixOutput <- renderPrint({
    if (!is.null(results$cm)) {
      print(results$cm)
    } else {
      cat(" ")
    }
  })
  
  output$featureImportanceOutput <- renderPrint({
    if (!is.null(results$var_imp)) {
      print(results$var_imp)
    } else {
      if (input$model_name == "K nearest neighbors") {
        cat("該模型不支援特徵重要性排序\n")
      } else {
        cat(" ")
      }
    }
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


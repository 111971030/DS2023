library(shiny)
library(shinyjs)
library(tidyverse)
library(caret)
library(e1071)
library(rpart)
library(randomForest)
library(pROC)

Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Load the feature.R file
source("feature.R")

# Read the train.csv file
train_data <- read_csv("train.csv")

# Preprocess the train data
preprocess_train_data <- preprocess_data(train_data)

# UI
ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs
  
  titlePanel("Modeling"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("model_name", "Select model", choices = c("Logistic regression", "K nearest neighbors", "SVC Linear", "SVC RBF", "Gaussian Naive Bayes", "Decision Tree", "Random Forest Classifier")),
      actionButton("runButton", "Run")
    ),
    
    mainPanel(
      shinyjs::hidden(
        h4("Training Predition", id = "preditionHeader"),
        uiOutput("trainingPredOutput"),
      
        h4("Confusion Matrix", id = "confusionMatrixHeader"),
        plotOutput("confusionMatrixOutput"),
      
        h4("Feature Importance", id = "featureImportanceHeader"),
        plotOutput("featureImportanceOutput"),
        verbatimTextOutput("featureNoImportanceOutput"),
        
        h4("ROC", id = "rocHeader"),
        plotOutput("rocOutput")
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  #=========== Get prediction information ===========
  get_pred_info <- function(training_model, train_data) {
    # Predict on the training dataset
    res_predictions <- predict(training_model, newdata = train_data)
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
    res_predictions_prob <- predict(training_model, newdata = train_data, type = "prob")[, 2]
    roc_obj <- roc(train_data$Survived, res_predictions_prob, quiet = TRUE)
    res_auc <- roc_obj$auc
    
    pred_info <- c(
      accuracy = round(res_accuracy, digits = 4),
      F1 = round(res_f1_score, digits = 4),
      precision = round(res_precision, digits = 4),
      recall = round(res_recall, digits = 4),
      auc = round(res_auc, digits = 4)
    )
    
    return(list(pred_info = pred_info, roc_obj = roc_obj))
  }
  
  #=========== Training the model ===========
  TrainingModel <- function(model_name, raw_data) {
    # Split the data into training , validation and test sets
    spec = c(train = .8, test = .1, valid = .1)
    
    data_train_sample = sample(cut(
      seq(nrow(preprocess_train_data)), 
      nrow(preprocess_train_data)*cumsum(c(0,spec)),
      labels = names(spec)
    ))
    
    splited_data = split(preprocess_train_data, data_train_sample)
    
    train_data <- splited_data$train
    valid_data <- splited_data$valid
    test_data <- splited_data$test
    
    # Set up the trainControl object for 5-fold cross-validation
    ctrl <- trainControl(method = "cv", number = 5, savePredictions = "all", classProbs = TRUE)
    
    if (model_name == "Logistic regression") {
      training_model <- train(Survived ~ ., data = train_data, method = "glm", trControl = ctrl, family = "binomial")
    } else if (model_name == "K nearest neighbors") {
      training_model <- train(Survived ~ .,data = train_data,method = "knn",trControl = ctrl,preProcess = c("center", "scale"),tuneLength = 3)
    } else if (model_name == "SVC Linear") {
      training_model <- train(Survived ~ ., data = train_data, kernel = "svmLinear", trControl = ctrl, scale = FALSE)
    } else if (model_name == "SVC RBF") {
      training_model <- train(Survived ~ ., data = train_data, kernel = "svmRadial", trControl = ctrl, scale = FALSE)
    } else if (model_name == "Gaussian Naive Bayes") {
      training_model <- train(Survived ~ ., data = train_data, kernel = "naiveBayes", trControl = ctrl)
    } else if (model_name == "Decision Tree") {
      training_model <- train(Survived ~ ., data = train_data, method = "rpart", trControl = ctrl)
    } else if (model_name == "Random Forest Classifier") {
      training_model <- train(Survived ~ ., data = train_data, method = "rf", trControl = ctrl)
    }
    
    # Get prediction information
    training_pred <- get_pred_info(training_model, train_data)
    valid_pred <- get_pred_info(training_model, valid_data)
    test_pred <- get_pred_info(training_model, test_data)
    
    df_pred <- rbind(training_pred$pred_info, valid_pred$pred_info ,test_pred$pred_info)
    
    # Generate predictions
    res_predictions <- predict(training_model, newdata = train_data)
    res_cm <- table(res_predictions, train_data$Survived)
    
    if (model_name == "K nearest neighbors") {
      importance_df <- NULL
    } else {
      # Plot importance list
      importance <- varImp(training_model)$importance
      importance <- importance[order(importance$Overall, decreasing = TRUE), , drop = FALSE]
      importance$Feature <- rownames(importance)
      # Rename the "Sex_factors" feature to "Sex"
      # Rename the "Embarked_factors" feature to "Embarked"
      importance$Feature <- ifelse(importance$Feature == "Sex_factors", "Sex", importance$Feature)
      importance$Feature <- ifelse(importance$Feature == "Embarked_factors", "Embarked", importance$Feature)
      importance_df <- as.data.frame(importance)
      importance_df$Feature <- factor(importance_df$Feature, levels = importance_df$Feature)
    }
    
    return(list(pred = df_pred, cm = res_cm, imp = importance_df, roc = training_pred$roc_obj))
  }
  
  results <- reactiveValues(pred = NULL, cm = NULL, imp = NULL)
  
  # Trigger event when click runButton button
  observeEvent(input$runButton, {
    shinyjs::hide("preditionHeader")
    shinyjs::hide("trainingPredOutput")
    shinyjs::hide("confusionMatrixHeader")
    shinyjs::hide("confusionMatrixOutput")
    shinyjs::hide("featureImportanceHeader")
    shinyjs::hide("featureImportanceOutput")
    shinyjs::hide("featureNoImportanceOutput") 
    shinyjs::hide("rocHeader")
    shinyjs::hide("rocOutput")
    
    model_name <- input$model_name
    model_results <- TrainingModel(model_name, preprocess_train_data)
    results$pred <- model_results$pred
    results$cm <- model_results$cm
    results$imp <- model_results$imp
    results$roc <- model_results$roc
    
    shinyjs::show("preditionHeader")
    shinyjs::show("trainingPredOutput")
    shinyjs::show("confusionMatrixHeader")
    shinyjs::show("confusionMatrixOutput")
    shinyjs::show("featureImportanceHeader")
    if (!is.null(results$imp)) {
      shinyjs::show("featureImportanceOutput")
      shinyjs::hide("featureNoImportanceOutput") 
    }else{
      shinyjs::hide("featureImportanceOutput")
      shinyjs::show("featureNoImportanceOutput") 
    }
    shinyjs::show("rocHeader")
    shinyjs::show("rocOutput")
    
  })
  
  customized_df  <- reactive({
    if (!is.null(results$pred)) {
      df_pred <- results$pred
      rownames(df_pred) <- c("Training data", "Validation data", "Test data")
      colnames(df_pred) <- c("Accuracy", "F1 Score", "Precision", "Recall", "AUC")
      df_pred
    } else {
      NULL
    }
  })
  
  output$trainingPredOutput <- renderTable({
    customized_df()
  }, rownames = TRUE)
  

  output$confusionMatrixOutput <- renderPlot({
    if (!is.null(results$cm)) {
      ggplot(as.data.frame(results$cm), aes(res_predictions, Var2, fill = Freq)) +
        geom_tile() +
        geom_text(aes(label = Freq), color = "black", size = 4) +
        scale_fill_gradient(low = "white", high = "steelblue") +
        labs(x = "Prediction", y = "Accuracy")
    }else{
      NULL
    }
  })
  
  
  output$featureImportanceOutput <- renderPlot({
    if (!is.null(results$imp)) {
      ggplot(results$imp, aes(x = Feature, y = Overall)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(x = "Feature", y = "Importance") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      NULL
    }
  })
  
  output$featureNoImportanceOutput <- renderText({
    if (input$model_name == "K nearest neighbors") {
      "This model does not support feature importance ranking.\n"
    }
  })
  
  output$rocOutput <- renderPlot({
    if (!is.null(results$roc)) {
      par(pty="s")
      plot(results$roc, col = "steelblue", legacy.axes = TRUE, print.auc = TRUE)
    } else {
      NULL
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

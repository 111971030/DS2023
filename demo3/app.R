# init

if(!require('shiny')) {
  install.packages('shiny')
  library('shiny')
}
if(!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')
  suppressPackageStartupMessages(library(tidyverse))
}
if(!require('caret')) {
  install.packages('caret')
  library('caret')
}
if(!require('e1071')) {
  install.packages('e1071')
  library('e1071')
}
if(!require('rpart')) {
  install.packages('rpart')
  library('rpart')
}

library(randomForest)
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# 匯入source
source("feature_sophia.R")
source("model_sophia.R")


# function
# 讀取原始資料的函數
train_data <- read_csv("train.csv")
# 資料前處理的函數
train_data_process <- preprocess_data(train_data)

# 訓練模型的函數
Logistic_regression_model <- TrainingModel("Logistic regression", train_data_process)
K_nearest_neighbors_model <- TrainingModel("K nearest neighbors", train_data_process)
SVC_Linear_model <- TrainingModel("SVC Linear", train_data_process)
Gaussian_Naive_Bayes_model <- TrainingModel("Gaussian Naive Bayes", train_data_process)
Decision_Tree_model <- TrainingModel("Decision Tree", train_data_process)
Random_Forest_Classifier_model <- TrainingModel("Random Forest Classifier", train_data_process)



# #範例 如何把一個新乘客的資料輸入，並得預測結果
# newpassenger <- data.frame(Name = "Sophia", 
#                           Pclass = 5, 
#                           Sex = "male", 
#                           Age = 30, 
#                           SibSp = 1, 
#                           Parch = 0, 
#                           Fare = 71, 
#                           Embarked = "D" )
# survival <- predicted_survival(Logistic_regression_model, newpassenger, train_data)


# UI
ui <- fluidPage(  
  # App title ----
  titlePanel("玩玩看 Can You Survive?"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    sidebarPanel(
      # Input:
      # Model
      selectInput("model_name", "Select model", choices = c("Logistic regression", 
                                                            "K nearest neighbors",
                                                            "SVC Linear",
                                                            "Gaussian Naive Bayes",
                                                            "Decision Tree",
                                                            "Random Forest Classifier"
      )),
      
      # Passenger Information: Name = "Sophia", Pclass = 1, Sex = "female", Age = 30, SibSp = 1, Parch = 0, Fare = 71, Embarked = "C" 
      textInput("input_name", label = "Name", value = "Jack"),
      uiOutput("input_Pclass"),
      radioButtons("input_Sex",
                   label = "Gender:",
                   choices = list("male" = "male","female" = "female"),
                   selected = "male"),
      sliderInput(inputId = "input_Age",
                  label = "Age:",
                  min = 1,
                  max = 100,
                  value = 30),
      sliderInput(inputId = "input_SibSp",
                  label = "SibSp:",
                  min = 0,
                  max = 10,
                  value = 1),
      sliderInput(inputId = "input_Parch",
                  label = "Parch:",
                  min = 0,
                  max = 10,
                  value = 0),
      textInput("input_Fare", label = "Fare", value = 71),
      uiOutput("input_Embarked"),
      
      actionButton("Ex1Button", "Example 1"),
      actionButton("Ex2Button", "Example 2"),
      
      actionButton("runButton", "Run")
    ),
    
    # outputs
    mainPanel(
      #titlePanel("Survive:"),
      textOutput('text1'),
      #textOutput('text2'),
      uiOutput("text2"),
      #textOutput('text3'),
      tags$head(tags$style("#text1{color: black;
                                 font-size: 24px;
                                 font-style: italic;
                                 }"
      )
      )
    )
  )
)
# server
server <- function(input, output, session) {
  
  modelname_list <- c("Logistic regression", 
                      "K nearest neighbors",
                      "SVC Linear",
                      "Gaussian Naive Bayes",
                      "Decision Tree",
                      "Random Forest Classifier")
  
  # Pclass dropdown list
  PclassData <- list("Pclass"=c(unique(train_data$Pclass)))
  PclassData <- lapply(PclassData,sort)
  
  output$input_Pclass <- renderUI ({ 
    selectInput(inputId = "input_Pclass", label = "Pclass",
                choices = c(PclassData))
  })
  
  # Embarked dropdown list
  EmbarkedData <- list("Embarked"=c(unique(train_data$Embarked)))
  EmbarkedData <- lapply(EmbarkedData,sort)
  
  output$input_Embarked <- renderUI ({ 
    selectInput(inputId = "input_Embarked", label = "Embarked",
                choices = c(EmbarkedData))
  })
  
  observeEvent(input$Ex1Button,{ # Survived
    updateSelectInput(session, "model_name", choices = modelname_list, selected = "Logistic regression")
    model_name <- "Logistic regression"
    
    #input$input_name <- "Jack"
    updateTextInput(session, "input_name",
                    label = "Name",
                    value = "Jack")
    name <- "Jack"
    
    #input$input_Pclass <- "1"
    updateSelectInput(inputId = "input_Pclass", label = "Pclass",
                      choices = c(PclassData),
                      selected = "1")
    pclass <- as.integer("1")
    
    #input$input_Sex <- "male"
    updateRadioButtons(session, "input_Sex",
                       label = "Gender:",
                       choices = list("male" = "male","female" = "female"),
                       selected = "male")
    sex <- "male"
    
    #input$input_Age <- "19" 1-100
    updateSliderInput(session = session, "input_Age", value = "19", min = 1, max = 100)
    age <- as.integer("19")
    
    #input$input_SibSp <- "0" 0-10
    updateSliderInput(session = session, "input_SibSp", value = "0", min = 0, max = 10)
    sibsp <- as.integer("0")
    
    #input$input_Parch <- "1" 0-10
    updateSliderInput(session = session, "input_Parch", value = "1", min = 0, max = 10)
    parch <- as.integer("1")
    
    #input$input_Fare <- "50"
    updateTextInput(session, "input_Fare",
                    label = "Fare",
                    value = "50")
    fare <- as.integer("50")
    
    #input$input_Embarked <- "Q"
    updateSelectInput(inputId = "input_Embarked", label = "Embarked",
                      choices = c(EmbarkedData),
                      selected = "Q")
    embarked <- "Q"
    
    
    newpassenger <- data.frame(Name = name,
                               Pclass = pclass,
                               Sex = sex,
                               Age = age,
                               SibSp = sibsp,
                               Parch = parch,
                               Fare = fare,
                               Embarked = embarked )
    
    # output$text3 <- renderText(paste(model_name,
    #                                  newpassenger$Name,
    #                                  newpassenger$Pclass,
    #                                  newpassenger$Sex,
    #                                  newpassenger$Age,
    #                                  newpassenger$SibSp,
    #                                  newpassenger$Parch,
    #                                  newpassenger$Fare,
    #                                  newpassenger$Embarked))
    
    if (model_name == "Logistic regression") {
      survival <- as.integer(predicted_survival(Logistic_regression_model, newpassenger, train_data))
    }else if (model_name == "K nearest neighbors") {
      survival <- as.integer(predicted_survival(K_nearest_neighbors_model, newpassenger, train_data))
    }else if (model_name == "SVC Linear") {
      survival <- as.integer(predicted_survival(SVC_Linear_model, newpassenger, train_data))
    }else if (model_name == "Gaussian Naive Bayes") {
      survival <- as.integer(predicted_survival(Gaussian_Naive_Bayes_model, newpassenger, train_data))
    }else if (model_name == "Decision Tree") {
      survival <- as.integer(predicted_survival(Decision_Tree_model, newpassenger, train_data))
    }else if (model_name == "Random Forest Classifier") {
      survival <- as.integer(predicted_survival(Random_Forest_Classifier_model, newpassenger, train_data))
    }else {
      survival <- as.integer(predicted_survival(Logistic_regression_model, newpassenger, train_data))
    }
    
    #survival <- as.integer(predicted_survival(model_name, newpassenger, train_data))
    if (survival == 1) {
      survival_text <- "Oh No ~~ You Died!."
      color_text <- "red"
    }else if (survival == 2) {
      survival_text <- "Congratulations!!!!!!!!!!!! You survived.."
      color_text <- "green"
    }
    message_text <- paste("Hi",name,"will you survive ?")
    output$text1 <- renderText(message_text)
    #output$text2 <- renderText(survival_text)
    # output$text2 <- renderText({
    #                             paste('<span style=\"color:', color_text, 
    #                                   '\">', survival_text, 
    #                                   ' </span>', sep = "")
    #                           })
    output$text2 <- renderUI(
      HTML(
        if (survival == 1) {
          as.character(div(style="color: red;font-size: 36px;", survival_text))
        }else if (survival == 2) {
          as.character(div(style="color: green;font-size: 36px;", survival_text))
        }
      ))
  })
  
  observeEvent(input$Ex2Button,{ # Died
    updateSelectInput(session, "model_name", choices = modelname_list, selected = "SVC Linear")
    model_name <- "SVC Linear"
    
    #input$input_name <- "Hank"
    updateTextInput(session, "input_name",
                    label = "Name",
                    value = "Hank")
    name <- "Hank"
    
    #input$input_Pclass <- "1"
    updateSelectInput(inputId = "input_Pclass", label = "Pclass",
                      choices = c(PclassData),
                      selected = "1")
    pclass <- as.integer("1")
    
    #input$input_Sex <- "male"
    updateRadioButtons(session, "input_Sex",
                       label = "Gender:",
                       choices = list("male" = "male","female" = "female"),
                       selected = "male")
    sex <- input$input_Sex
    
    #input$input_Age <- "30" 1-100
    updateSliderInput(session = session, "input_Age", value = "30", min = 1, max = 100)
    age <- as.integer("30")
    
    #input$input_SibSp <- "4" 0-10
    updateSliderInput(session = session, "input_SibSp", value = "4", min = 0, max = 10)
    sibsp <- as.integer("4")
    
    #input$input_Parch <- "2" 0-10
    updateSliderInput(session = session, "input_Parch", value = "2", min = 0, max = 10)
    parch <- as.integer("2")
    
    #input$input_Fare <- "50"
    updateTextInput(session, "input_Fare",
                    label = "Fare",
                    value = "50")
    fare <- as.integer("50")
    
    #input$input_Embarked <- "Q"
    updateSelectInput(inputId = "input_Embarked", label = "Embarked",
                      choices = c(EmbarkedData),
                      selected = "Q")
    embarked <- "Q"
    
    
    newpassenger <- data.frame(Name = name,
                               Pclass = pclass,
                               Sex = sex,
                               Age = age,
                               SibSp = sibsp,
                               Parch = parch,
                               Fare = fare,
                               Embarked = embarked )
    
    # output$text3 <- renderText(paste(model_name,
    #                                  newpassenger$Name,
    #                                  newpassenger$Pclass,
    #                                  newpassenger$Sex,
    #                                  newpassenger$Age,
    #                                  newpassenger$SibSp,
    #                                  newpassenger$Parch,
    #                                  newpassenger$Fare,
    #                                  newpassenger$Embarked))
    
    if (model_name == "Logistic regression") {
      survival <- as.integer(predicted_survival(Logistic_regression_model, newpassenger, train_data))
    }else if (model_name == "K nearest neighbors") {
      survival <- as.integer(predicted_survival(K_nearest_neighbors_model, newpassenger, train_data))
    }else if (model_name == "SVC Linear") {
      survival <- as.integer(predicted_survival(SVC_Linear_model, newpassenger, train_data))
    }else if (model_name == "Gaussian Naive Bayes") {
      survival <- as.integer(predicted_survival(Gaussian_Naive_Bayes_model, newpassenger, train_data))
    }else if (model_name == "Decision Tree") {
      survival <- as.integer(predicted_survival(Decision_Tree_model, newpassenger, train_data))
    }else if (model_name == "Random Forest Classifier") {
      survival <- as.integer(predicted_survival(Random_Forest_Classifier_model, newpassenger, train_data))
    }else {
      survival <- as.integer(predicted_survival(Logistic_regression_model, newpassenger, train_data))
    }
    
    #survival <- as.integer(predicted_survival(model_name, newpassenger, train_data))
    if (survival == 1) {
      survival_text <- "Oh No ~~ You Died!"
      color_text <- "red"
    }else if (survival == 2) {
      survival_text <- "Congratulations!!!!!!!!!!!! You survived."
      color_text <- "green"
    }
    message_text <- paste("Hi",name,"will you survive ?")
    output$text1 <- renderText(message_text)
    #output$text2 <- renderText(survival_text)
    # output$text2 <- renderText({
    #                             paste('<span style=\"color:', color_text, 
    #                                   '\">', survival_text, 
    #                                   ' </span>', sep = "")
    #                           })
    output$text2 <- renderUI(
      HTML(
        if (survival == 1) {
          as.character(div(style="color: red;font-size: 36px;", survival_text))
        }else if (survival == 2) {
          as.character(div(style="color: green;font-size: 36px;", survival_text))
        }
      ))
  })
  
  observeEvent(input$runButton,{
    model_name <- input$model_name
    
    name <- input$input_name
    pclass <- as.integer(input$input_Pclass)
    sex <- input$input_Sex
    age <- as.integer(input$input_Age)
    sibsp <- as.integer(input$input_SibSp)
    parch <- as.integer(input$input_Parch)
    fare <- as.integer(input$input_Fare)
    embarked <- input$input_Embarked
    
    
    newpassenger <- data.frame(Name = name,
                               Pclass = pclass,
                               Sex = sex,
                               Age = age,
                               SibSp = sibsp,
                               Parch = parch,
                               Fare = fare,
                               Embarked = embarked )
    
    # output$text3 <- renderText(paste(model_name,
    #                                  newpassenger$Name,
    #                                  newpassenger$Pclass,
    #                                  newpassenger$Sex,
    #                                  newpassenger$Age,
    #                                  newpassenger$SibSp,
    #                                  newpassenger$Parch,
    #                                  newpassenger$Fare,
    #                                  newpassenger$Embarked))
    
    if (model_name == "Logistic regression") {
      survival <- as.integer(predicted_survival(Logistic_regression_model, newpassenger, train_data))
    }else if (model_name == "K nearest neighbors") {
      survival <- as.integer(predicted_survival(K_nearest_neighbors_model, newpassenger, train_data))
    }else if (model_name == "SVC Linear") {
      survival <- as.integer(predicted_survival(SVC_Linear_model, newpassenger, train_data))
    }else if (model_name == "Gaussian Naive Bayes") {
      survival <- as.integer(predicted_survival(Gaussian_Naive_Bayes_model, newpassenger, train_data))
    }else if (model_name == "Decision Tree") {
      survival <- as.integer(predicted_survival(Decision_Tree_model, newpassenger, train_data))
    }else if (model_name == "Random Forest Classifier") {
      survival <- as.integer(predicted_survival(Random_Forest_Classifier_model, newpassenger, train_data))
    }else {
      survival <- as.integer(predicted_survival(Logistic_regression_model, newpassenger, train_data))
    }
    
    #survival <- as.integer(predicted_survival(model_name, newpassenger, train_data))
    if (survival == 1) {
      survival_text <- "Oh No ~~ You Died!"
      color_text <- "red"
    }else if (survival == 2) {
      survival_text <- "Congratulations!!!!!!!!!!!! You survived."
      color_text <- "green"
    }
    message_text <- paste("Hi",name,"will you survive ?")
    output$text1 <- renderText(message_text)
    #output$text2 <- renderText(survival_text)
    # output$text2 <- renderText({
    #                             paste('<span style=\"color:', color_text, 
    #                                   '\">', survival_text, 
    #                                   ' </span>', sep = "")
    #                           })
    output$text2 <- renderUI(
      HTML(
        if (survival == 1) {
          as.character(div(style="color: red;font-size: 36px;", survival_text))
        }else if (survival == 2) {
          as.character(div(style="color: green;font-size: 36px;", survival_text))
        }
      ))
  })
  
}
# Shiny app
shinyApp(ui = ui, server = server)
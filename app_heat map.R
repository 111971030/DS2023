library(shiny)
library(ggplot2)
library(reshape2)

if(!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')
}

Titanic <- read_csv("Titanic/train.csv")

dimensions <- dim(Titanic)
num_rows <- dimensions[1]  
num_cols <- dimensions[2]  
cat("row:",num_rows,"\n","col:",num_cols,"\n")

sapply(Titanic, function(x) sum(is.na(x)))

Titanic_train <- subset(Titanic, select = -c(PassengerId, Name, Ticket, Cabin))
Titanic_train <- Titanic_train[complete.cases(Titanic_train[,c("Embarked","Age")]),]

dimensions <- dim(Titanic_train)
num_rows <- dimensions[1] 
num_cols <- dimensions[2] 
cat("row:",num_rows,"\n","col:",num_cols,"\n")

str(Titanic_train)

unique(Titanic_train[c("Sex")])
Titanic_train$Sex_factors <- as.numeric(factor(Titanic_train$Sex))

unique(Titanic_train[c("Embarked")])

Titanic_train$Embarked_factors <- as.numeric(factor(Titanic_train$Embarked))

Titanic_train_subset <- subset(Titanic_train, select = -c(Sex, Embarked))

str(Titanic_train_subset)

Titanic_train_subset_scale <- Titanic_train_subset
Titanic_train_subset_scale[2 : 8] <- as.data.frame(scale(Titanic_train_subset_scale[2 : 8]))

head(Titanic_train_subset_scale)

set.seed(1)
spec = c(train = .8, test = .1, validate = .1)

Titanic_train_sample = sample(cut(
  seq(nrow(Titanic_train_subset_scale)), 
  nrow(Titanic_train_subset_scale)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(Titanic_train_subset_scale, Titanic_train_sample)




ui <- fluidPage(
  titlePanel("Feature Correlation Heatmap"),
  mainPanel(
    plotOutput("heatmapPlot")
  )
)

server <- function(input, output) {
  output$heatmapPlot <- renderPlot({
    correlation <- cor(res$train[, 2:8])
    correlation_melt <- melt(correlation)
    
    ggplot(data = correlation_melt, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      geom_text(aes(label = round(value, 2)), color = "black") +  
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(x = "Features", y = "Features", fill = "Correlation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)


library(shiny)
Sys.setlocale("en_US.UTF-8")
if(!require('ggplot2')) {
  install.packages('ggplot2')
  library('ggplot2')
}
if(!require('gridExtra')) {
  install.packages('gridExtra')
  library('gridExtra')
}
if(!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')
}

# Read data
df <- read_csv("Titanic/train.csv", locale = readr::locale(encoding = "UTF-8"))

# Set features
features <- c("Pclass", "Sex", "SibSp", "Parch", "Embarked")


ui <- fluidPage(
  # navbar
  navbarPage(
    title = "",
    tabPanel("Information", 
             fluidPage(
               fluidRow(
                 column(4, 
                        selectInput("features", "Select feature:", choices = features, selected = features[1], multiple = TRUE)
                 ),
                 column(8, 
                        plotOutput("plot",width = "400px", height = "400px")
                 )
               )
             )
    ),
    tabPanel("Modeling", 
             fluidPage(
               
             )
    ),
    tabPanel("Result",
             fluidPage(
               
             )
    )
  )
)

server <- function(input, output) {

  output$plot <- renderPlot({
    selected_features <- input$features
    
    plots <- lapply(selected_features, function(feature) {
      plot <- ggplot(df, aes(x = feature)) +
        geom_bar(aes(fill = factor(Survived)), position = 'dodge') +
        labs(title = feature, x = NULL, y = NULL) +
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              legend.position = 'right', legend.title = element_blank(),
              axis.text = element_text(size = 16, face = "bold"))
      print(plot)
    })
    
    grid.arrange(grobs = plots, ncol = 1)
    
  })
  
}

shinyApp(ui, server)

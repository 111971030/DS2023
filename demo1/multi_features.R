library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# Read data
df <- read.csv("train.csv")

# Set features
features <- c("Pclass", "Sex", "SibSp", "Parch", "Embarked")

# Shiny ui
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("features", "Select features:", choices = features, selected = features[1:2]),
      numericInput("digits", "Number of digits:", value = 3, min = 0, max = 10)
    ),
    mainPanel(
      plotOutput("plot"),
      br(),
      DT::dataTableOutput("table")
    )
  )
)

# Shiny server
server <- function(input, output) {
  result_data <- eventReactive(input$features, {
    selected_features <- input$features
    digits <- input$digits
    
    if (length(selected_features) > 0) {
      # Calculate survival rate by selected features
      result <- df %>%
        group_by(!!!syms(selected_features)) %>%
        summarise(SurvivalRate = mean(Survived))
      
      result
    } else {
      # If no features are selected, return an empty data frame
      data.frame()
    }
  })
  
  output$plot <- renderPlot({
    result <- result_data()
    
    if (nrow(result) > 0) {
      # Plot the survival rate with labels
      plot <- ggplot(result, aes(x = interaction(!!!syms(input$features)), y = SurvivalRate, fill = interaction(!!!syms(input$features)))) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = round(SurvivalRate, digits = input$digits), y = SurvivalRate), vjust = -0.5, size = 3) +
        labs(title = "Survival Rate by Selected Features", x = paste(input$features, collapse = " & "), y = "Survival Rate") +
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              legend.position = 'right', legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
              axis.text = element_text(size = 12, face = "bold"))
      
      print(plot)
    } else {
      # If no features are selected, show a default message
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), main = "No features selected")
    }
  })
  
  output$table <- DT::renderDataTable({
    result <- result_data()
    
    # Return the result as a DataTable with additional options for styling and functionality
    datatable(result, options = list(
      dom = 'Bfrtip',
      buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
      pageLength = 10
    ))
  })
}

# Run Shiny app
shinyApp(ui, server)

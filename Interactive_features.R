library(shiny)
library(ggplot2)
library(gridExtra)

# Read data
df <- read_csv("train.csv")

# Set features
features <- c("Pclass", "Sex", "SibSp", "Parch", "Embarked")

# Shiny ui
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("features", "Select feature:", choices = features, selected = features[1], multiple = TRUE)
    ),
    mainPanel(
      plotOutput("plot",width = "800px", height = "800px")
    )
  )
)

# Shiny server
server <- function(input, output) {
  output$plot <- renderPlot({
    selected_features <- input$features
    
    plots <- lapply(selected_features, function(feature) {
      plot <- ggplot(df, aes_string(x = feature)) +
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

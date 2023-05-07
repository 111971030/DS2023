library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

Titanic <- read_csv("Titanic/train.csv")

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Information", tabName = "Information", icon = icon("th")),
      menuItem("Modeling", tabName = "Modeling", icon = icon("gear")),
      menuItem("Result", tabName = "Result", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Information",
              fluidRow(
                column(4, 
                       h2("Information left"),
                       
                ),
                column(8, 
                       h2("Information right"),
                       DT::dataTableOutput("mytable")
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "Modeling",
              h2("Modeling tab content")
      ),
      
      # Third tab content
      tabItem(tabName = "Result",
              h2("Result tab content")
      )
    )
  )
)


server <- function(input, output) {
  #count NA values in each column
  Titanic_NA <- sapply(Titanic, function(x) sum(is.na(x)))
  
  output$mytable <- DT::renderDataTable(Titanic_NA,
                                        options = list(scrollX = TRUE),
                                        rownames = FALSE)
  
}

shinyApp(ui, server)

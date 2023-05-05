library(shiny)
library(shinydashboard)

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
                       h2("Information right")
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


server <- function(input, output) { }

shinyApp(ui, server)
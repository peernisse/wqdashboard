## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title='Water Quality Explore'),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Data Import", tabName = "dataimport", icon = icon("th"))
    )
      
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "home",
              h2('Water Quality Explorer'),
              h3('Overview'),
              h3('Disclaimer')
      ),
      
      # Second tab content
      tabItem(tabName = "dataimport",
              h2("Data Import Instructions"),
              h2('Required Data Format')
      )
    )
  )
)

server <- function(input, output) { }

shinyApp(ui, server)
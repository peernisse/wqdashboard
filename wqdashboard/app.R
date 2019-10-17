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
      # Home tab content
      tabItem(tabName = "home",
              h2('Water Quality Explorer'),
              h3('Overview'),
              h3('Disclaimer')
      ),
      
      # Import tab content
      tabItem(tabName = "dataimport",
              h2("Data Import Instructions"),
              h2('Required Data Format'),
              hr(),
              h2('Data Import'),
              fluidRow(
                column(6,
                       
                       fileInput("file", buttonLabel = 'Choose File',label=NULL,placeholder = 'Loading may take some time',accept='.csv'))
              )
      )
    )
  )
)

server <- function(input, output) { }

shinyApp(ui, server)
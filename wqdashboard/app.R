## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title='Water Quality Explore',
                  
                  dropdownMenu(type = "tasks", badgeStatus = "success",
                               taskItem(value = 90, color = "green",
                                        "Documentation"
                               ),
                               taskItem(value = 17, color = "aqua",
                                        "Project X"
                               ),
                               taskItem(value = 75, color = "yellow",
                                        "Server deployment"
                               ),
                               taskItem(value = 80, color = "red",
                                        "Overall project"
                               )
                    )#Task dropdown
                  
                  
                  ),#Dashboarrd header
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Data Import", tabName = "dataimport", icon = icon("th")),
      menuItem('Plots',tabName='plots',icon=icon('chart-bar')),
      menuItem("Tables",tabName = 'tables',icon=icon('file-alt')),
      menuItem("Regression Tools",tabName = 'regression',icon=icon('chart-line')),
      menuItem("Map Tools",tabName = 'maptools',icon=icon('map'))
    )#Sidebar menu
      
  ),#dashboardSidebar
  dashboardBody(
    tabItems(
      # Home tab content
      tabItem(tabName = "home",
              h2('Water Quality Explorer'),
              h3('Overview'),
              h3('Disclaimer'),
              h3('References')
      ),#Home tab
      
      # Import tab content
      tabItem(tabName = "dataimport",
              h2("Data Import Instructions"),
              h2('Required Data Format'),
              hr(),
              h2('Data Import'),
              fluidRow(
                column(6,fileInput("file", buttonLabel = 'Choose File',label=NULL,placeholder = 'Loading may take some time',accept='.csv'))
              )#File import row
      ),#Import tab
      
      # Plotting tab
      tabItem(tabName = 'plots',
        h2('Plotting Tools'),
        fluidRow(
          box('Time Series'),
          box('Boxplots')
          
        ),#plots row1
        
        fluidRow(
          box('QQ Plots'),
          box('Histogram',
              plotOutput("distPlot"),
              sliderInput("bins", "Number of Bins:", 1, 50, 30)
              )
          
        )#plots row2
        
      ),#Plotting tab
      
      #Tables tab
      tabItem(tabName = 'tables',
              h2('Table Tools')
              
              ),#Tables tab
      
      #Regression tab
      tabItem(tabName = 'regression',
              h2('Regression Tools')
              
      ),#Tables tab
      
      #Tables tab
      tabItem(tabName = 'tables',
              h2('Table Tools')
              
      ),#Map tab
      
      #Regression tab
      tabItem(tabName = 'maptools',
              h2('Map Tools')
              
      )#Map tab
      
    )#Dashboard body
  )#????
)#Dashboard page?

server <- function(input, output,session) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  
  
  #File input load--------------
  pData <- reactive({
    
    if(input$demoLoad == 0) {
      inFile <- input$file
    }
    
    if(input$demoLoad > 0){
      
      inFile<-data.frame(
        name='demoData',
        size=1413000,
        type='csv',
        datapath='./data/demoData.csv'
      )
      
      inFile$datapath<-as.character(inFile$datapath)
    }
    
    
    if (is.null(inFile))
      return(NULL)
    
    
    #Read in table
    tbl <- read.csv(inFile$datapath, header=TRUE, stringsAsFactors = FALSE)
    #Fix date format
    tbl$Date<-as.POSIXct(strptime(tbl$Date,format="%d-%b-%y"))
    #Fix unit cases
    tbl$Units<-fixUnits(tbl)
    #Add units to parameter column
    tbl$Parameter<-paste0(tbl$Parameter,' (',tbl$Units,')')
    #Make non detect substitution columns
    tbl$Result_ND<-as.numeric(ifelse(tbl$DetectionFlag=='ND',tbl$ReportingLimit*0.5,tbl$Value))
    tbl$NonDetect<-as.numeric(ifelse(tbl$DetectionFlag=='ND',tbl$ReportingLimit*0.5,''))
    
    return(tbl)
  })
  
  
}

shinyApp(ui, server)
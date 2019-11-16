## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
source('helpers.R')
options(scipen = 6)

#Data import testing read in
#tbl<-read.csv('./wqdashboard/data/testData.csv',stringsAsFactors = FALSE)

#Start App--------------------------
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
  dashboardSidebar(width=400,
    #h3('Pages',style = "margin-left:5px;"),
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data Import", tabName = "dataimport", icon = icon("th")),
      menuItem('Water Quality Criteria',tabName='wqc',icon = icon('tint')),
      menuItem('Plots',tabName='plots',icon=icon('chart-bar')),
      menuItem("Tables",tabName = 'tables',icon=icon('file-alt')),
      menuItem("Regression Tools",tabName = 'regression',icon=icon('chart-line')),
      menuItem("Map Tools",tabName = 'maptools',icon=icon('map'))
    ),#Sidebar menu
    tags$hr(color='white',width="95%"),
    h3('Filter Tools',style = "margin-left:5px;"),
    fluidRow(
        column(6,
               h5(strong('Choose Matrices',style = "margin-left:5px;")),
               actionLink('selectall_Matrix','Select All | Clear All'),
               wellPanel(id='sitePanel',style = "overflow-y:scroll; max-height: 180px; margin-left:5px",
                         uiOutput('choose_matrix',style = "color:black;")
               )
               
        ),
        
        column(6,
               h5(strong('Choose Sites')),
               actionLink('selectall_Sites','Select All | Clear All'),
               wellPanel(id='sitePanel',style = "overflow-y:scroll; max-height: 180px; margin-right:5px",
                         uiOutput('choose_sites',style = "color:black;")
               )
               
        )
      
    ),#fluid row
    
    fluidRow(
        column(6,
               h5(strong("Choose Locations",style = "margin-left:5px;")),
               actionLink("selectall_Locs","Select All | Clear All"),
               
               wellPanel(id='locPanel',style = "overflow-y:scroll; max-height: 180px;margin-left:5px",
                         uiOutput('choose_locs',style = "color: black;")
                         
               )
               
        ),
        column(6,
               h5(strong("Choose Parameters")),
               actionLink('selectall_Params','Select All | Clear All'),
               wellPanel(id='paramPanel',style = "overflow-y:scroll; max-height: 180px;margin-right:5px",
                         uiOutput('choose_params',style = "color:black;")
                         
               )
               
        )
    ),#fluid row
    
    h5(strong("Choose Date Range",style = "margin-left:5px")),
    fluidRow(
      column(8,uiOutput('choose_dates')),
      column(1,uiOutput('reset_dates'))
    )
      
  ),#dashboardSidebar
  
  ###########PAGES#############################
  #############################################
  
  dashboardBody(
    tabItems(
      # Home tab content
      tabItem(tabName = "home",
              h2('Water Quality Explorer'),
              p('This application is a generic environemntal data analysis tool. It is designed for use with censored and/or uncensored water, groundwater,
                soil, and air data.'),
              p('A focus of the application is comparison of environmental data to local or federal regulatory standards, either point by ponit, or in a statistical manner (e.g., confidence intervals, control charts).'),
              h3('Overview'),
              tags$ul(
                tags$li('Import data from CSV'),
                tags$li('Exploration plots'),
                tags$li('Reporting table generation'),
                tags$li('Regression analysis tools'),
                tags$li('Mapping data locations')
              ),
              h3('Disclaimer'),
              p('This application is intended for exploratory data analysis. Any statistical tools/tests should be used
                at the discretion of the user, and the user is responsible for any conclusions or decisions made as a result
                of statistical analysis results.'),
              tags$img(src='testtoooh.jpg'),
              h3('References'),
              tags$ul(
              tags$li('[R base 2019]. R Core Team (2018). R: A language and environment for statistical computing. R Foundation for
  Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.'),
              tags$li('[ggplot2]. H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.'),
              tags$li('Bruce, P. and Bruce, A. "Practical Statistics for Data Scientists". Oreilly (2017). ISBN:978-1-491-95296-2.')
              )
              
      
      ),#Home tab
      
      # Import tab content
      tabItem(tabName = "dataimport",
              
              column(6,
                     h2('Data Import Instructions'),
                     p('Upfront text'),
                     box(width=12,
                       h2('Data Import'),
                       fileInput("file", buttonLabel = 'Choose File',label=NULL,placeholder = 'Loading may take some time',accept='.csv'),
                       tags$em('Use Demo Data'),
                       actionButton(inputId = 'demoLoad',label = 'Load Demo Data')
                     )
              ),#first column,
                       
              column(6,
                h2("Required Data Format"),
                p('This application is designed to temporarily load the user\'s data file 
                  into memory for the duration of application use. User data are not 
                  retained on the server after application use.'),
                p('The following information describes the required data format.'),
                
                tags$ul(
                  
                  tags$li('This app is hosted on the secure R Shinyapps.io server at https://peernisse/shinyapps.io/wqdashboard/.'),
                  tags$li('If you have R installed and use R, you can download this app from https://github.com/peernisse/wqdashboard/.'),
                  tags$li('The data to be analyzed must be in a `.csv` file in a local directory.'),
                  tags$li('The data are not transfered to the app server to preserve data security.')
                ),
                
                h3('Your Data File Should Contain the Folowing Columns'),
                h3('Example Table Structure')
              )#Second column,
              
             
              

      ),#Import tab
      
      # Water quality criteria tab
      tabItem(tabName='wqc',
              h2('Configure Water Quality Criteria')),#Water quality criteria tab
      
      # Plotting tab
      tabItem(tabName = 'plots',
        h2('Plotting Tools'),
          column(width=12,
                 
                 fluidRow(
                   h4('Time Series',style = "margin-left:10px;"),
                   box(width=12,collapsible = TRUE,collapsed = TRUE)
                          ),
                 fluidRow(
                   h4('Boxplots',style = "margin-left:10px;"),
                   box(width=12,collapsible = TRUE,collapsed = TRUE)
                          
                          ),
                 
                 
                 fluidRow(
                   h4('Probability Plots',style = "margin-left:10px;"),
                   box(width=12,collapsible = TRUE,collapsed = TRUE)
                          
                          ),
                 fluidRow(
                   h4('Histograms',style = "margin-left:10px;"),
                   box(width=12,collapsible = TRUE,collapsed = TRUE,
                     plotOutput("distPlot"),
                     sliderInput("bins", "Number of Bins:", 1, 50, 30)
                    )
                 
                 )   
                 
            )#column
          
          
        
        
      ),#Plotting tab
      
      #Tables tab
      tabItem(tabName = 'tables',
              h2('Table Tools'),
              dataTableOutput('tblData')
              ),#Tables tab
      
      #Regression tab
      tabItem(tabName = 'regression',
              h2('Regression Tools')
              
      ),#Regression tab
     
      #Map tab
      tabItem(tabName = 'maptools',
              h2('Map Tools')
              
      )#Map tab
      
    )#Dashboard body
  )#????
)#Dashboard page?

###########################################################
#Server side section---------------------------------------
###################################

server <- function(input, output,session) {
  
  #######################################################
  #Placeholder histogram to be removed
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  #######################################################
  #TESTING DATA INPUT
  
  
  #####################################################
  #######################################################
  ####################################################

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

        datapath='./data/testData.csv'

      )

      inFile$datapath<-as.character(inFile$datapath)
      
    }
    
    
    if (is.null(inFile))
      return(NULL)
    
    
    #Read in table
    tbl <- read.csv(inFile$datapath, header=TRUE, stringsAsFactors = FALSE)
    
    #Fix date format

    #tbl$Date<-as.POSIXct(strptime(tbl$sample_date,format="%d/%b/%y"))
    #Fix unit cases
    #tbl$Units<-fixUnits(tbl)
    #Add units to parameter column
    #tbl$Parameter<-paste0(tbl$Parameter,' (',tbl$Units,')')
    #Make non detect substitution columns
    #tbl$Result_ND<-as.numeric(ifelse(tbl$DetectionFlag=='ND',tbl$ReportingLimit*0.5,tbl$Value))
    #tbl$NonDetect<-as.numeric(ifelse(tbl$DetectionFlag=='ND',tbl$ReportingLimit*0.5,''))

    tbl$Date<-as.POSIXct(strptime(tbl$Date,format="%m/%d/%Y"))
    #Fix unit cases
    tbl<-fixUnits(tbl)
    #Create names for total and dissolved instead of just letters
    tbl <- tbl %>%
      mutate(PREP_CODE = case_when(
        
        fraction == 'D' ~ 'Dissolved',
        fraction == 'T' ~ 'Total',
        TRUE ~ fraction
        
      ),
      Parameter=paste0(chemical_name,', ',PREP_CODE,' (',UNITS,')'),
      RESULT=case_when(
        
        is.na(result_text) ~ 0,
        TRUE ~ result_text
        
      ),
      RESULT_ND = case_when(
        
        detect_flag== 'Y' ~ RESULT,
        TRUE ~ 0.5*(reporting_detection_limit)
        
      ),
      NDS = case_when(
        
        detect_flag == 'N' ~ 0.5*(reporting_detection_limit)
        
      )
        )

    
   return(tbl)
  })
  
  #Get min and max dates
  baseDates <- reactive({
    if(input$demoLoad == 0) {
      inFile <- input$file
    }
    
    if(input$demoLoad > 0){
      
      inFile<-data.frame(
        name='demoData',
        size=1413000,
        type='csv',
        datapath='./data/testData.csv'
      )
      
      inFile$datapath<-as.character(inFile$datapath)
    }
    
    if (is.null(inFile))
      return(NULL)
    
    dts <- read.csv(inFile$datapath, header=TRUE, stringsAsFactors = FALSE)
    dts$Date<-as.POSIXct(strptime(dts$Date,format="%m/%d/%Y"))
    bDates<-c(min(dts$Date),max(dts$Date))
    
    return(bDates)#This could possibly combined into pData() as a separate output and referenced pData()$bDates
    
  })
  
  
#Data table ouput----------------------------
  # output$tblData<-renderDataTable({
  # 
  #   # if(is.null(input$file))
  #   #   return()
  # 
  #   dtable<-pData()
  # 
  #   return(dtable)
  # 
  # })#output tblData
  
  
  #Buttons-------------------------------------------------
  #Date range refresh button action
  observeEvent(input$clrDates,{
    
    updateDateRangeInput(session,inputId = 'dtRng', start = baseDates()[1], end = baseDates()[2])
  })
  
  #Create refresh date range button
  output$reset_dates <- renderUI({
    if(is.null(pData()))
      return()
    actionButton('clrDates',NULL,
                 icon("refresh"), 
                 style="color: #fff; background-color: #337ab7; 
                  border-color: #2e6da4;font-size:90%; width:35px; 
                 height:30px;margin-top:10px; ")
  })
  
  
  #Pickers----------------------------------------------------
  
  #Create location picker
  #This loads up by default with no choices available
  output$choose_locs<-renderUI({

    if(is.null(pData()))
      return()
    locs<-sort(unique(pData()$Location))

    checkboxGroupInput('locids',NULL,
                       choices = locs,
                       selected = NULL)
    
    
    
  })
  
  #THis listens for values selected in other checkbox groups and changes accordingly
  observe({

    if(is.null(pData()))
      return()
    lulocs<-pData() %>% filter(Site %in% input$sts,Matrix %in% input$mtrx)
    locs<-sort(unique(lulocs$Location))



    if(input$selectall_Locs == 0)
    {
      updateCheckboxGroupInput(session,"locids",NULL,choices=locs,selected=NULL)
    }
    else if (input$selectall_Locs%%2 == 0)
    {
      updateCheckboxGroupInput(session,"locids",NULL,choices=locs)
    }
    else
    {
      updateCheckboxGroupInput(session,"locids",NULL,choices=locs,selected=locs)
    }

  })


  #Create parameter picker
  observe({
    params<-sort(unique(pData()$Parameter))
    if(input$selectall_Params == 0) return(NULL)
    else if (input$selectall_Params%%2 == 0)
    {
      updateCheckboxGroupInput(session,"params",NULL,choices=params)
    }
    else
    {
      updateCheckboxGroupInput(session,"params",NULL,choices=params,selected=params)
    }
  })
  output$choose_params<-renderUI({
    if(is.null(pData()))
      return()
    params<-sort(unique(pData()$Parameter))

    checkboxGroupInput('params',NULL,
                       choices = params)
  })

  #Create matrix picker
  observe({
    matrices<-sort(unique(pData()$Matrix))
    if(input$selectall_Matrix == 0) return(NULL)
    else if (input$selectall_Matrix%%2 == 0)
    {
      updateCheckboxGroupInput(session,"mtrx",NULL,choices=matrices,inline = FALSE)
    }
    else
    {
      updateCheckboxGroupInput(session,"mtrx",NULL,choices=matrices,selected=matrices,inline = FALSE)
    }
  })

  output$choose_matrix<-renderUI({
    if(is.null(pData()))
      return()
    matrices<-sort(unique(pData()$Matrix))

    checkboxGroupInput('mtrx',NULL,
                       choices = matrices,
                       selected = matrices[1],
                       inline = FALSE)
  })

  #Create site picker
  observe({
    sites<-sort(unique(pData()$Site))
    if(input$selectall_Sites == 0) return(NULL)
    else if (input$selectall_Sites%%2 == 0)
    {
      updateCheckboxGroupInput(session,"sts",NULL,choices=sites,inline = FALSE)
    }
    else
    {
      updateCheckboxGroupInput(session,"sts",NULL,choices=sites,selected=sites,inline = FALSE)
    }
  })

  output$choose_sites<-renderUI({
    if(is.null(pData()))
      return()
    sites<-sort(unique(pData()$Site))

    checkboxGroupInput('sts',NULL,
                       choices = sites,
                       selected = sites[1],
                       inline = FALSE)
  })
  
  #Create date range input
  output$choose_dates<-renderUI({
    if(is.null(pData()))
      return()
    # minDate<-min(pData()$Date)
    # maxDate<-max(pData()$Date)
    
    dateRangeInput('dtRng',NULL,
                   start = baseDates()[1],
                   end = baseDates()[2],
                   format = 'dd-M-yyyy'
                   
    )
    
  })
  
  
  #Data table output-------------------
  #Output for long format data table
  
  output$tblData <- renderDataTable({
    if(is.null(input$locids))
      return()

    fData<-pData() %>% filter(Location %in% input$locids,Matrix %in% input$mtrx,
                              Date >= input$dtRng[1] & Date<=input$dtRng[2],
                              Parameter %in% input$params, Site %in% input$sts) %>%
      arrange(Location,Date)
    fData$Date<-as.character(fData$Date)

    return(unique(fData))
  })
  
  
}#Server component






shinyApp(ui, server)









## app.R ##
library(shiny)
library(DT)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(rpivotTable)
source('helpers.R')
options(scipen = 6)

#Data import testing read in
#tbl<-read.csv('./wqdashboard/data/testData.csv',stringsAsFactors = FALSE)

#Start App--------------------------
ui <- dashboardPage(
  
  #Start dashboard header----------
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
  
  #Start dashboard sidebar----------------
  dashboardSidebar(width=400,
    #h3('Pages',style = "margin-left:5px;"),
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data Import", tabName = "dataimport", icon = icon("th")),
      menuItem('Water Quality Criteria',tabName='wqc',icon = icon('tint')),
      menuItem('Plots',tabName='plots',icon=icon('chart-bar')),
      menuItem("Tables",tabName = 'tables',icon=icon('file-alt')),
      menuItem("Regression Tools",tabName = 'regression',icon=icon('chart-line')),
      menuItem("Statistical Tests",tabName = 'statstests',icon=icon('calculator')),
      menuItem("Map Tools",tabName = 'maptools',icon=icon('map'))
    ),#Sidebar menu
    tags$hr(color='white',width="95%"),
    h3('Filter Tools',style = "margin-left:5px;"),
    h5(strong("Date Range",style = "margin-left:5px")),
    fluidRow(
      column(8,uiOutput('choose_dates')),
      column(1,uiOutput('reset_dates'))
    ),
    
    fluidRow(
        column(6,
               h5(strong('Matrices',style = "margin-left:5px;")),
               actionLink('selectall_Matrix','Select All | Clear All'),
               wellPanel(id='sitePanel',style = "overflow-y:scroll; max-height: 180px; margin-left:5px",
                         uiOutput('choose_matrix',style = "color:black;")
               )
               
        ),
        
        column(6,
               h5(strong('Sites')),
               actionLink('selectall_Sites','Select All | Clear All'),
               wellPanel(id='sitePanel',style = "overflow-y:scroll; max-height: 180px; margin-right:5px",
                         uiOutput('choose_sites',style = "color:black;")
               )
               
        )
      
    ),#fluid row
    
    fluidRow(
        column(6,
               h5(strong("Locations",style = "margin-left:5px;")),
               actionLink("selectall_Locs","Select All | Clear All"),
               
               wellPanel(id='locPanel',style = "overflow-y:scroll; max-height: 180px;margin-left:5px",
                         uiOutput('choose_locs',style = "color: black;")
                         
               )
               
        ),
        column(6,
               h5(strong("Parameters")),
               actionLink('selectall_Params','Select All | Clear All'),
               wellPanel(id='paramPanel',style = "overflow-y:scroll; max-height: 180px;margin-right:5px",
                         uiOutput('choose_params',style = "color:black;")
                         
               )
               
        )
    )#fluid row
    
    
      
  ),#dashboardSidebar
  
  #Start dashboard body-----------
  
  dashboardBody(
    tabItems(
      # Home tab content----------------
      tabItem(tabName = "home",

              
              
              fluidRow(
                column(width=6,h2('Water Quality Explorer')
                       ),#column
                
                column(width=6,tags$img(src='testtoooh.jpg')
                )#column
                
              ),#fluid row
              
              
              p('This application is a generic environmental data analysis tool. It is designed for use with censored and/or uncensored water, groundwater,
                soil, and air data.',style="margin-top:20px;"),

              p('A focus of the application is comparison of environmental data to local or federal regulatory standards, either point by ponit, or in a statistical manner (e.g., confidence intervals, control charts).'),
              
              
              h3('Overview'),
              tags$ul(
                tags$li('Import data from CSV'),
                tags$li('Exploration plots'),
                tags$li('Reporting table generation'),
                tags$li('Regression analysis tools'),
                tags$li('Mapping data locations')
              ),
              
              h3("Try the App"),
              p('Load the demo data, select some locations and parameters from the left menu.'),
              tags$em('Use Demo Data'),
              actionButton(inputId = 'demoLoad',label = 'Load Demo Data'),
              
              
              h3('Disclaimer'),
              p('This application is intended for exploratory data analysis. Any statistical tools/tests should be used
                at the discretion of the user, and the user is responsible for any conclusions or decisions made as a result
                of statistical analysis results.'),
              
              h3('References'),
              tags$ul(
              tags$li('[R base 2019]. R Core Team (2018). R: A language and environment for statistical computing. R Foundation for
  Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.'),
              tags$li('[ggplot2]. H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.'),
              tags$li('Bruce, P. and Bruce, A. "Practical Statistics for Data Scientists". Oreilly (2017). ISBN:978-1-491-95296-2.')
              )
              
      
      ),#Home tab
      
      # Import tab content------------------
      tabItem(tabName = "dataimport",
              
              fluidRow(
                
                column(6,
                       h2('Data Import Instructions'),
                       p('Data are added by uploading a CSV file or Configuring connection to a database source such as SQL Server or MS Access.'),
                       box(width=12,
                           h2('Data Import'),
                           tags$em('If "Choose File does not appear, 
                                   file upload is disabled for Demo Version. 
                                   Use "Load Demo Data" on the Home Tab.'),
                           #actionButton(inputId = 'demoLoad2',label = 'Load Demo Data'),
                           fileInput("file", buttonLabel = 'Choose File',label=NULL,placeholder = 'Loading may take some time',accept='.csv')#,
                           #tags$em('Or Use Demo Data Button on Home Tab')
                           #actionButton(inputId = 'demoLoad',label = 'Load Demo Data')
                       ),
                       box(width=12,
                           h2('Data Connection'),
                           p('Pending')
                           #fileInput("file", buttonLabel = 'Choose File',label=NULL,placeholder = 'Loading may take some time',accept='.csv'),
                           
                           #actionButton(inputId = 'demoLoad',label = 'Load Demo Data')
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
                       )
                       
                )#Second column,
                
              ),#end fluid row
              
              fluidRow(
                
                h3('Your Data File Should Contain the Folowing Columns'),
                tableOutput('fields'),
               
                h3('Example Table Structure') 
                
              )#End fluid row
        
      ),#Import tab
      
      # Water quality criteria tab-------------------
      tabItem(tabName='wqc',
              h2('Configure Water Quality Criteria'),
              fluidRow(
                column(6,
                  h3('Select Environmental Limits Column'),
                  p('The data in this column can be used to flag 
                    values in tables and to draw horizontal lines on plots'),
                  uiOutput('limits')
                  
                ),#End column
                column(6,
                  p('Pending Area')       
                )#End column
                
              )#end fluidrow
              
      ),#Water quality criteria tab
      
      # Plotting tab---------------------
      tabItem(tabName = 'plots',
        h2('Plotting Tools'),
          
        fluidRow(
          
          column(width = 6,
                 h4('Swap Faceting Variables'),
                 uiOutput('tsFacetSwap')

                 ),#End facet swap column
          
          column(width = 6,
                 h4('Set Scales Fixed or Auto'),
                 uiOutput('scalesSet')
                 
                 ),#End scales fixed or free column
          hr()
       ),#End fluid row plot tools tools
        
        
        fluidRow(
          column(width=12,
                 
                 
                 fluidRow(
                   
                   box(title='Time Series',width=12,collapsible = TRUE,collapsed = FALSE,
                       
                       
                       h4('Click Plot Points for More Info'),
                       dataTableOutput("plot_clickinfo"),
                       plotOutput("timeSeriesPlot",click = 'tsClick')
                       
                       
                       
                   )#box ts
                   
                 ),#fluid row,
                 
                 
                 fluidRow(
                   
                   box(title='Boxplots',width=12,collapsible = TRUE,collapsed = FALSE,
                       h4('Click Plot Boxes for More Info'),
                       dataTableOutput("bplot_clickinfo"),
                       plotOutput("boxPlot",click = 'bxClick')
                   )#box
                   
                 ),#fluid row,
                 
                 
                 fluidRow(
                   
                   box(title='Probability Plots',width=12,collapsible = TRUE,collapsed = FALSE,
                       
                       plotOutput("qPlot")
                       
                       
                   )#box
                   
                 ),#fluid row
                 
                 fluidRow(
                   
                   box(title='Histograms',width=12,collapsible = TRUE,collapsed = FALSE,
                       plotOutput("hPlot"),
                       uiOutput('histSlider')
                   )#box
                   
                 )#end fluid row plot boxes
                 
          )#End plotting boxes column
          
          
        )#End plots container row
          
    ),#Plotting tab
      
      #Tables tab----------------------
      tabItem(tabName = 'tables',
                box(title="Pivot Table",width=12,collapsible = TRUE,collapsed = TRUE,style="overflow-x: scroll;
                      overflow-y: scroll;",
                      
                    rpivotTableOutput('pvtTable')
                      
                      
                  ),
                box(title="Filtered Data Table",width=12,collapsible = TRUE,collapsed = TRUE,style="overflow-x: scroll;
                    overflow-y: scroll;",
                    
                    #dataTableOutput('tblData')
                    DTOutput('tblData')
                    
                    
                )#box,
              
              
              ),#Tables tab
      
      #Regression tab-------------------
      tabItem(tabName = 'regression',
              
              box(title='Regression Tools',width=12,collapsible = TRUE,collapsed = FALSE,style="padding:35px;",
                  
                  fluidRow(
                    
                    p('This tool allows for linear regression of 
                      selected variables. Select the independent (x)
                      variable and the dependent (y) variable. Deselect points using clicking or drag selection, and the "Toggle Points" button.')
                    ),
                  fluidRow(
                    
                    column(3,h3("X Axis Variable"),uiOutput('regX')),
                    column(3,h3('Y Axis Variable'),uiOutput('regY'))
                  ),
                  fluidRow(
                    
                    plotOutput('rPlot',
                               click = "rPlot_click",
                               
                               brush = brushOpts(
                                 id = "rPlot_brush"
                                 
                               )#brushopts
                               
                    )#plotoutput
                  ),#fluid row
                  
                  fluidRow(
                    actionButton("exclude_toggle", "Toggle points",style="margin-left:8px;"),
                    actionButton("exclude_reset", "Reset")     
                    
                  ),
                  
                  fluidRow(
                    h3('Model Statistics'),
                    tableOutput('rTbl')
                    
                  )
                  
                  
                  )#box
              
              
      ),#Regression tab
     
      
      #Stats tab------------------
      tabItem(
        tabName = 'statstests',
        fluidRow(
          
          column(12, h3('Select Stats to Show'),
                 #actionLink('selectall_Stats','Select All | Clear All'),
                 uiOutput('choose_stats')
                 
                 )#,#end column
          
          # column(6,h3('Output this Table to CSV'),
          #        fluidRow(
          #          column(6,textInput('expStatsFilename',label=NULL,width = '200px',placeholder = 'enter filename')),
          #          column(1,h4('.csv'),style='padding-left: 0px;'),
          #          column(2,downloadButton('expStats','Download'))
          #          
          #        )#End fluid row
          # )#End column
        ),#End fluid row
        
        hr(),
        
       DTOutput('statsData')
        
        
        
      ),#stats tab
      
    #Map tab------------------------
      tabItem(tabName = 'maptools',
              h2('Map Explore'),
              fluidRow(
                column(width = 2,
                       h3('Map Tools')
                       ),#column1
                
                column(width=10,
                       h3('Site Map'),
                        
                       
                       leafletOutput(
                         'map',height = 600,width = 650
                       )

                       )#column2
                
              ),#fluidRow
              
              tags$hr(style='border-color:black;'),
              h2('References'),
              tags$ul(
                tags$li('[Mapping with `leaflet`]. Joe Cheng, Bhaskar Karambelkar and Yihui Xie (2019). leaflet: Create Interactive Web Maps with the JavaScript
                        `Leaflet` Library. R package version 2.0.3. https://CRAN.R-project.org/package=leaflet')
              )
              
              
              
      )#Map tab
      
    )#Dashboard body
  )#????
)#Dashboard page?

#'##########################################################
#Server side section---------------------------------------
#'##################################

server <- function(input, output,session) {
  
  options(shiny.maxRequestSize=30*1024^2)
  
  
  #'####################################################
  #'######################################################
  #'###################################################

  #File input load--------------
  pData <- reactive({
    
    
    if(input$demoLoad == 0) {
      inFile <- input$file
    }
    
    #if(input$demoLoad > 0 | input$demoLoad2 > 0){
    if(input$demoLoad > 0 ){

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

    #Set up limits column if it has been selected
    
  #   print(input$limcol)#Testing
  # 
  #   tbl$Limits<-tbl[,input$limcol]
  # 
  # print(names(tbl))
  #   
    
   
    
    #Fix date format to be date
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
        
      ),
      DetectionFlag = case_when(
        detect_flag == 'N' ~ 'ND',
        detect_flag == 'Y' ~ '=',
        TRUE ~ ''
      ),
      LATITUDE=as.numeric(LATITUDE),
      LONGITUDE=as.numeric(LONGITUDE)
        
    )#mutate

    
   return(tbl)
  })
  
  #Get min and max dates-------------------
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
  
  
  #Data table output-------------------
  #Output for long format data table
  
  output$tblData <- renderDT({
    if(is.null(input$locids))
      return()
    
    fData<-pData() %>% filter(Location %in% input$locids,Matrix %in% input$mtrx,
                              Date >= input$dtRng[1] & Date<=input$dtRng[2],
                              Parameter %in% input$params, Site %in% input$sts) %>%
      arrange(Location,Date)
    fData$Date<-as.character(fData$Date)
    
    fData %>% 
      datatable(.,extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               exportOptions = list(title = NULL),
                               buttons = c('copy', 'csv', 'excel', 'pdf')
                               
                               )
      )#end datatable
    
    #return(unique(fData))
  })
  
  #Output for wide format table----
  
  
  #Output for example field name table----
  output$fields<-renderTable({
    fields
  })
  
  
  
  #Pivot table output----------
  output$pvtTable<-renderRpivotTable({
    
    if(is.null(input$locids))
      return()
    
   pvtData<-pData() %>% filter(Location %in% input$locids,Matrix %in% input$mtrx,
                              Date >= input$dtRng[1] & Date<=input$dtRng[2],
                              Parameter %in% input$params, Site %in% input$sts) %>%
      arrange(Location,Date)
   pvtData$Date<-as.character(pvtData$Date)
    
    rpivotTable(unique(pvtData))
    
  })
  
  #Dropdowns-------------------
  #Swap plot faceting variable picklist
  output$tsFacetSwap<-renderUI({
    pickerInput('tsSwap',choices = c('Parameter','Location'),
                selected='Parameter') 
    
  })
  
  #Create plot scales free or fixed picklist
  output$scalesSet<-renderUI({
    
    
    pickerInput('sclsChoices',
                choices = c('Free All',
                            'Fix All',
                            'Free Y, X Fixed',
                            'Free X, Y Fixed'
                ),
                selected = 'Free All')
  })#End output scalesSet
  
  #Create field picker for environmental imits column
  output$limits<-renderUI({
    if(is.null(pData()))
      return()
    
    pickerInput('limcol',choices = c('None',names(pData())),
                selected= 'None') 
    
  })#end limits picker
  
  #Buttons-------------------------------------------------
  #Date range refresh button action
  observeEvent(input$clrDates,{
    
    updateDateRangeInput(session,inputId = 'dtRng', start = baseDates()[1], end = baseDates()[2])
  })
  
  #Create refresh date range button
  output$reset_dates <- renderUI({
    
    actionButton('clrDates',NULL,
                 icon("refresh"), 
                 style="color: #fff; background-color: #337ab7; 
                  border-color: #2e6da4;font-size:90%; width:35px; 
                 height:30px;margin-top:10px; ")
  })
  
  #Sliders-----------------------------------
  output$histSlider<-renderUI({
    if(is.null(pData()))
      return()
    
    sliderInput("ggBins",
                "Number of bins:",
                min = 10,
                max = 100,
                value = 30,
                width = '50%')
    
  })
  
  
  #Stats tab items--------------------------------
  #Create stats table column picker-----------------
  # 
  # observe({
  #   statList<-names(statSumm())[-c(1:3)]
  # 
  #   if(input$selectall_Stats == 0) return(NULL)
  #   else if (input$selectall_Stats%%2 == 0)
  #   {
  #     updateCheckboxGroupInput(session,"stats",NULL,choices=statList,inline = TRUE)
  #   }
  #   else
  #   {
  #     updateCheckboxGroupInput(session,"stats",NULL,choices=statList,selected=statList,inline = TRUE)
  #   }
  # })
  
  output$choose_stats<-renderUI({
    if(is.null(statSumm()))
      return()
    
    statList<-names(statSumm())[-c(1:3)]
    #statList<-statList[-c(1:3)]
    
    checkboxGroupInput('stats',NULL,
                       choices = statList,
                       selected = statList,
                       inline = TRUE)
    
  })
  
  
  
  #Stats summary data table output based on checkbox selection---------------------------
  output$statsData <- renderDT({
    if(is.null(input$locids)|is.null(input$stats))
      return()
    sData <- statSumm() %>% select(1:3,input$stats)
    #sData <- statSumm()
    #return(sData)
    
    sData %>% 
      datatable(.,extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               exportOptions = list(title = NULL),
                               buttons = c('copy', 'csv', 'excel', 'pdf')
                               
                )
      )#end datatable
    
  })
  
  #Making raw stats summary table
  statSumm <- reactive({
    
    pData() %>% filter(Location %in% input$locids,Matrix %in% input$mtrx,
                       Date >= input$dtRng[1] & Date<=input$dtRng[2],
                       Parameter %in% input$params) %>% 
      group_by(Matrix, Parameter,Location) %>% 
      summarise(`Number of Observations`=length(RESULT_ND),
                `Percent Non-Detect`=round(perND(DetectionFlag,'ND'),0),
                `Minimum Date`=as.character(min(Date)),
                `Maximum Date`=as.character(max(Date)),
                Minimum=as.character(ifelse(min(RESULT_ND)==0,'ND',min(RESULT_ND))),
                Maximum=max(RESULT_ND),
                `First Quartile`=as.character(ifelse(quantile(RESULT_ND,0.25)==0,'ND',quantile(RESULT_ND,0.25))),
                `Third Quartile`=as.character(ifelse(quantile(RESULT_ND,0.75)==0,'ND',quantile(RESULT_ND,0.75))),
                Average=mean(RESULT_ND),
                `Standard Deviation`=sd(RESULT_ND),
                Variance=var(RESULT_ND))

    
  })
  
 
  
  #Export stats summary table download button------------------
  #Output handler
  # output$expStats <- downloadHandler(
  #   
  #   filename = function() {
  #     paste(input$expStatsFilename, ".csv", sep = "")
  #   },
  #   
  #   content = function(file) {
  #     write.csv(statSumm(), file, row.names = FALSE)
  #   }
  # )#downloadhandler
  
  #Map dataframe------------------------------
  
  output$map <- renderLeaflet({
    if(is.null(pData()))
      return()
    
    maplocs<-pData() %>% select(Location,LATITUDE,LONGITUDE) %>% 
      filter(Location %in% input$locids) %>% unique(.)
    #mCenter<- c(mean(maplocs$LATITUDE),mean(maplocs$LONGITUDE))
    #mBounds<-c(max(maplocs$LONGITUDE),min(maplocs$LONGITUDE),max(maplocs$LATITUDE),min(maplocs$LATITUDE))
    
    
    m <- leaflet() %>% 
      addTiles(group="Roads") %>% 
      addProviderTiles('Esri.WorldImagery',group="Satellite") %>% 
      #addProviderTiles("CartoDB.PositronOnlyLabels") %>% 
      #setView(mCenter[2], mCenter[1],zoom = 1) %>% 
      #setMaxBounds(mBounds[2],mBounds[4],mBounds[1],mBounds[3]) %>% 
      addCircles(data=maplocs,lng = ~LONGITUDE,lat = ~LATITUDE,label = ~Location, labelOptions = c(permanent = TRUE),
                       radius = 8, group = "Locations",stroke = FALSE,fillOpacity = 0.8,  fillColor='#ed7000') %>%  
      addScaleBar(position='bottomright') %>% 
      addMeasure(
        position = "topright",
        primaryLengthUnit = "feet",
        primaryAreaUnit = "acres",
        activeColor = "#3D535D",
        completedColor = "#7D4479",
        localization = "en"
      ) %>% #add measure
      addLayersControl(
        baseGroups = c("Satellite","Roads"),
        overlayGroups = c("Locations"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    
    return(m)
    
  })
  
  
  
  #Pickers----------------------------------------------------
  
  #Create location picker
  #This loads up by default with first location picked just so folks are not confused
  output$choose_locs<-renderUI({

    if(is.null(pData()))
      return()
    locs<-sort(unique(pData()$Location))

    checkboxGroupInput('locids',NULL,
                       choices = locs,
                       selected = locs[1])
    
    
    
  })
  
  #THis listens for values selected in other checkbox groups and changes accordingly
  observe({

    if(is.null(pData()))
      return()
    lulocs<-pData() %>% filter(Site %in% input$sts,Matrix %in% input$mtrx)
    locs<-sort(unique(lulocs$Location))



    if(input$selectall_Locs == 0)
    {
      updateCheckboxGroupInput(session,"locids",NULL,choices=locs,selected=locs[1])
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
                       choices = params,
                       selected = params[1])
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
  
  
  
  #Create regression tool parameter picker-----
  observe({
    
    #if(is.null(input$params)) return(NULL)
    
    #if (input$selectall_Params == 0) return(NULL)
    
    if (input$selectall_Params%%2 == 0)
    {
      updatePickerInput(session,'regrX',choices='')
      updatePickerInput(session,'regrY',choices='')
    }
    
    else if(input$selectall_Params%%2 == 1)
    {
      updatePickerInput(session,'regrX',choices=c('Date',input$params),
                        selected='Date')
      updatePickerInput(session,'regrY',choices=input$params)
    } 
    
    
    
  })
  
  
  
  output$regX<-renderUI({
    pickerInput('regrX',choices=c('Date',input$params))
    
  })
  
  output$regY<-renderUI({
    pickerInput('regrY',choices=input$params)
    
  })
  
  
  
  #Plots---------------------------
  
  #Timeseries plots------
  output$timeSeriesPlot <- renderPlot({
    if(is.null(input$locids))
      return()
    
    if(input$tsSwap=='Parameter'){
      tsFacet<-'Parameter'
      col<- 'Location'
      
    }
    if(input$tsSwap=='Location'){
      tsFacet<-'Location'
      col<- 'Parameter'
      
    }
    
    if(input$sclsChoices == 'Free All'){scls<-'free'}
    if(input$sclsChoices == 'Fix All'){scls<-'fixed'}
    if(input$sclsChoices == 'Free Y, X Fixed'){scls<-'free_y'}
    if(input$sclsChoices == 'Free X, Y Fixed'){scls<-'free_x'}
    
    
    
    tsData<-as.data.frame(filter(pData(),Location %in% input$locids,
                                 Date >= input$dtRng[1] & Date<=input$dtRng[2],
                                 Parameter %in% input$params,
                                 Matrix %in% input$mtrx))
    #Make table of limits
    myTest<-"Not working"
    
    if(!is.null(input$limcol)){
      myTest<-input$limcol
    }
    
    
    
    # if(input$limcol != "None"){
    #   limitscolumn<-pData()[.input$limcol]
    #   
    #   limData<-tsData %>% 
    #     group_by(Parameter) %>% 
    #     summarize(Limit = min(limitscolumn))
    # }
    # 
    # print(limData)
    
    #tsp<-tsPlot(tsData,tsFacet,col)
    
    #return(tsp)
    
    #testing ggplot logic inside this instead of function
    ggplot(tsData,aes(x=Date,y=RESULT_ND))+
      geom_line(aes_string(colour=col),size=0.5)+
      geom_point(aes_string(colour=col),size=3)+
      geom_point(aes(x=Date,y=NDS,fill='Non-Detect at 1/2 MDL'),shape=21,size=2)+
      scale_fill_manual(values='white')+
      facet_wrap(as.formula(paste('~',tsFacet)),scales=scls)+
      theme(legend.position = "bottom", legend.title = element_blank())+
      theme(strip.background = element_rect(fill = '#727272'),strip.text = element_text(colour='white',face='bold',size = 12))+
      labs(x="Date",y="Value",
           title="Time Series Non-Detects Hollow at 1/2 the Reporting Limit",
           subtitle = myTest)+
      theme(plot.title = element_text(face='bold',size=14))
    
    
    
  })#renderPlot
  
  tsData2<-reactive({
    filter(pData(),Location %in% input$locids,
           Date >= input$dtRng[1] & Date<=input$dtRng[2],
           Parameter %in% input$params,
           Matrix %in% input$mtrx) %>% 
      select(Site,Location,Date,Matrix,Parameter,detect_flag,RESULT_ND,reporting_detection_limit,MDL)
  })
  
  #Time series info boxes
  output$plot_clickinfo <- renderDataTable({
    
    #nearPoints(pData(), input$tsClick,threshold = 10)
    
    res <- nearPoints(tsData2(), input$tsClick,threshold = 3,maxpoints = 1)
    
    if (nrow(res) == 0)
      return()
    res
  })
  
  
  #Boxplots------------
  
  output$boxPlot <- renderPlot({
    if(is.null(input$locids))
      return()
    
    if(input$tsSwap=='Parameter'){
      bxFacet<-'Parameter'
      bxCol<- 'Location'
      
    }
    if(input$tsSwap=='Location'){
      bxFacet<-'Location'
      bxCol<- 'Parameter'
      
    }
    
    
    if(input$sclsChoices == 'Free All'){scls<-'free'}
    if(input$sclsChoices == 'Fix All'){scls<-'fixed'}
    if(input$sclsChoices == 'Free Y, X Fixed'){scls<-'free_y'}
    if(input$sclsChoices == 'Free X, Y Fixed'){scls<-'free_x'}
    
    
    bxData<-as.data.frame(filter(pData(),Location %in% input$locids,
                                 Date >= input$dtRng[1] & Date<=input$dtRng[2],
                                 Parameter %in% input$params,
                                 Matrix %in% input$mtrx))
    # bxp<-bxPlot(bxData)
    # return(bxp)
    
    ggplot(bxData,aes(x=Location,y=RESULT_ND))+
      geom_boxplot(aes_string(fill=bxCol))+
      #geom_jitter(color="black")+
      #geom_jitter(aes(x=Location,y=NonDetect),color="white")+
      facet_wrap(as.formula(paste('~',bxFacet)), scales=scls)+
      theme(strip.background = element_rect(fill = '#727272'),strip.text = element_text(colour='white',face='bold',size = 12))+
      theme(legend.position = "bottom", legend.title = element_blank())+
      labs(x="Location",y="Value",title="Boxplots Non-Detects at 1/2 the Reporting Limit")+
      theme(plot.title = element_text(face='bold',size=14))
    
    
  })
  
  bxData2<-reactive({pData() %>% filter(Location %in% input$locids,
                                        Date >= input$dtRng[1] & Date<=input$dtRng[2],
                                        Parameter %in% input$params,
                                        Matrix %in% input$mtrx) %>% 
      group_by(Location,Parameter) %>% 
      summarize(
        
        `Percent Non-Detect`=round(perND(detect_flag,'N'),0),
        `Minimum Date`=min(Date),
        `Maximum Date`=max(Date),
        Minimum=as.character(ifelse(min(RESULT_ND)==0,'ND',min(RESULT_ND))),
        `First Quartile`=as.character(ifelse(quantile(RESULT_ND,0.25)==0,'ND',quantile(RESULT_ND,0.25))),
        Mean=mean(RESULT_ND),
        Median=median(RESULT_ND),
        `Third Quartile`=as.character(ifelse(quantile(RESULT_ND,0.75)==0,'ND',quantile(RESULT_ND,0.75))),
        Maximum=max(RESULT_ND),
        Value = mean(RESULT_ND)   
      ) %>% as.data.frame(.)
  })
  
  print(bxData2)
  
  #Boxplot info box table
  output$bplot_clickinfo <- renderDataTable({
    
    #nearPoints(pData(), input$tsClick,threshold = 10)
    
    bres <- nearPoints(bxData2(), input$bxClick,threshold = 50,maxpoints = 1)
    
    if (nrow(bres) == 0)
      return()
    bres
  })
  
  
  #Probablity plots---------------------------------
  output$qPlot <- renderPlot({
    if(is.null(input$locids))
      return()
    
    if(input$tsSwap=='Parameter'){
      qqFacet<-'Parameter'
      qqCol<- 'Location'
      
    }
    if(input$tsSwap=='Location'){
      qqFacet<-'Location'
      qqCol<- 'Parameter'
      
    }
    
    
    if(input$sclsChoices == 'Free All'){scls<-'free'}
    if(input$sclsChoices == 'Fix All'){scls<-'fixed'}
    if(input$sclsChoices == 'Free Y, X Fixed'){scls<-'free_y'}
    if(input$sclsChoices == 'Free X, Y Fixed'){scls<-'free_x'}
    
    
    qData<-as.data.frame(filter(pData(),Location %in% input$locids,
                                Date >= input$dtRng[1] & Date<=input$dtRng[2],
                                Parameter %in% input$params,
                                Matrix %in% input$mtrx))
    
    ggplot(qData,aes(sample=as.numeric(RESULT_ND)))+
      geom_qq(aes_string(color=qqCol))+
      geom_qq_line(aes_string(color=qqCol))+
      facet_wrap(as.formula(paste('~',qqFacet)), scales=scls)+
      theme(strip.background = element_rect(fill = '#727272'),strip.text = element_text(colour='white',face='bold',size = 12))+
      theme(legend.position = "bottom", legend.title = element_blank())+
      labs(x="Theoretical Distribution(normal)",y="Value",title="Distribution (quantile plot) Non-Detects at 1/2 the Reporting Limit")+
      theme(plot.title = element_text(face='bold',size=14))
  })
  
  #Histograms-----------------------------------------------
  output$hPlot <- renderPlot({
    if(is.null(input$locids))
      return()
    
    if(input$tsSwap=='Parameter'){
      hFacet<-'Parameter'
      hCol<- 'Location'
      
    }
    if(input$tsSwap=='Location'){
      hFacet<-'Location'
      hCol<- 'Parameter'
      
    }
    
    
    if(input$sclsChoices == 'Free All'){scls<-'free'}
    if(input$sclsChoices == 'Fix All'){scls<-'fixed'}
    if(input$sclsChoices == 'Free Y, X Fixed'){scls<-'free_y'}
    if(input$sclsChoices == 'Free X, Y Fixed'){scls<-'free_x'}
    
    hData<-as.data.frame(filter(pData(),Location %in% input$locids,
                                Date >= input$dtRng[1] & Date<=input$dtRng[2],
                                Parameter %in% input$params,
                                Matrix %in% input$mtrx))
    
    ggplot(hData,aes(x=RESULT_ND))+
      geom_histogram(aes_string(fill=hCol),alpha=0.5,bins = input$ggBins)+
      facet_wrap(as.formula(paste('~',hFacet)), scales=scls)+
      theme(strip.background = element_rect(fill = '#727272'),strip.text = element_text(colour='white',face='bold',size = 12))+
      theme(legend.position = "bottom", legend.title = element_blank())+
      labs(x="Value Bins=30",y="Count",title="Distribution (histogram) Non-Detects at 1/2 the Reporting Limit")+
      theme(plot.title = element_text(face='bold',size=14))
  })
  
  #Regression plot reactive dataframe setup------------------------
  
  pdata<-reactive({
    
    rData<-pData() %>%
      filter(Location %in% input$locids,
             Date >= input$dtRng[1] & Date<=input$dtRng[2],
             Parameter %in% input$params,
             Matrix %in% input$mtrx) %>%
      select(Location,Date,Parameter,RESULT_ND) %>%
      arrange(Location,Parameter,Date) %>%
      as.data.frame(.)
    
    if(input$regrX=='Date'){
      rX<-rData %>% 
        filter(Parameter == input$regrY) %>% 
        arrange(as.Date(Date)) %>% 
        pull(Date)
      
      rY<- rData %>% filter(Parameter == input$regrY) %>% 
        arrange(Date) %>% 
        pull(RESULT_ND)
      
      pdata<-data.frame(rX=rX,rY=rY)
      
      #xLab<-'Date'
      #yLab<-unique(input$regrY)
      
    } else
      
      if(input$regrX!='Date'){
        
        setup<-rData %>% 
          filter(Parameter %in% c(input$regrY,input$regrX)) %>% 
          select(Location,Date,Parameter,RESULT_ND)
        
        setup$Parameter<-factor(setup$Parameter,levels=c(input$regrY,input$regrX))
        
        pdata<-data.table::dcast(setup,Location+Date~Parameter,value.var='RESULT_ND',fun.aggregate=mean)
        
        pdata<-pdata %>% mutate(chk=complete.cases(.)) %>% filter(chk!=FALSE)
        
        names(pdata)<-c('Location','Date','rY','rX','CHECK')
        
        
      }
    
    return(pdata)
    
  })
  
  
  # For storing which rows have been excluded
  vals <- reactive({
    reactiveValues(
      keeprows = rep(TRUE, nrow(pdata()))
    )  
  })
  
  
  #Make the plot and output it
  output$rPlot<-renderPlot({
    if(is.null(input$regrX)) return()
    
    #Get axis values
    xLab<-unique(input$regrX)
    yLab<-unique(input$regrY)
    locs<-input$locids 
    locs<-paste(locs,collapse=', ')
    print(locs)
    
    # Plot the kept and excluded points as two separate data sets
    keep    <- pdata()[ vals()$keeprows, , drop = FALSE]
    exclude <- pdata()[!vals()$keeprows, , drop = FALSE]
    
    g<-ggplot(keep,aes(x=rX,y=rY))+
      geom_smooth(method='lm',aes(fill="Best-fit line and 95% Confidence Band"))+
      geom_point(size=2.5)+
      geom_point(data=exclude,size=2.5,shape = 21, fill = NA, color = "black", alpha = 0.25)+
      labs(x=xLab,y=yLab,title=paste('Regression of',yLab,'vs',xLab,'\nFor Location(s)',locs))+
      theme(legend.position = 'bottom',legend.title = element_blank())+
      scale_fill_manual(values=c('grey'))
    
    g
    
  })
  #
  #
  # Toggle points that are clicked
  
  observeEvent(input$rPlot_click, {
    vals<-vals()
    
    res <- nearPoints(pdata(), input$rPlot_click, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
    vals<-vals()
    
    res <- brushedPoints(pdata(), input$rPlot_brush, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # # Reset all points
  observeEvent(input$exclude_reset, {
    
    vals<-vals()
    
    vals$keeprows <- rep(TRUE, nrow(pdata()))
  })
  
  
  
  #Regression model output-------------------
  
  
  output$rTbl<-renderTable({
    pdata<-pdata()
    vals<-vals()
    
    # res <- brushedPoints(pdata(), input$rPlot_brush, allRows = TRUE)
    # 
    # vals$keeprows <- xor(vals$keeprows, res$selected_)
    
    mdata   <- pdata()[ vals$keeprows, , drop = FALSE]
    
    mdl<-lm(rY~rX,data=mdata)
    
    mdlstats<-broom::glance(mdl)
    mdlstats
    
  })
  
}#Server component






shinyApp(ui, server)









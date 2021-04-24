library(stats)
library(tidyverse) # this includes dplyr for data wranling, ggplot2 for plotting, and tidyr for reshaping data
library(shiny)
library(plotrix) # for standard error function
library(shinythemes)
library(gridExtra)
library(colourpicker)
library(shinyjs) 
library(shinydashboard) # install this for layour skeleton
library(shinydashboardPlus)


ui <- dashboardPage(

  
  dashboardHeader(title='LifeTrackR'),
  
  dashboardSidebar(
    
 

    #selector for graph type
    selectInput("graph", "Choose a type of graph:",
                choice=c("Default","Stacked","Stacked(percentage)","Linear"),
                selected = "Default",
                multiple = FALSE),
    uiOutput("loc"),#parameter selector holder
    checkboxInput("time", label = "Set range of timepoint", value = TRUE),
    uiOutput("time_range"),
    checkboxInput("set", label = "Set Treatment", value = FALSE),
    uiOutput("treat"),
    #actionButton("add","Add"),
    #actionButton("delete","Delete"),
    actionButton('plot_button','Plot')
  ), # end of sidebar
  
  ### The right tab panel #########
  # 1. for theme selector
  controlbar = dashboardControlbar(collapsed = TRUE, 
                                   minified = FALSE,
                                   title = 'Theme',
                                   skin='dark',
                                   skinSelector(),
                                   controlbarMenu(
                                     id = "menu",
                                     controlbarItem(
                                       "setting 1",
                                       "Welcome to tab 1"
                                     ),
                                     controlbarItem(
                                       "setting 2",
                                       "Welcome to tab 2"
                                     )
                                   )
                              
                                   ),
  
  ############### The body  ##################
  dashboardBody(
    
    # add css to the header
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
      '))),
    

    
    
    #tabset for plot, summary and table
    tabsetPanel(
      
      tabPanel("Plot", uiOutput('colors'),
               plotOutput("plot",inline=TRUE),
               #file upload
               box(
                 title = "Data",
                 collapsible = TRUE,
                 closable = TRUE,
                 conditionalPanel(   # logic: if file uploaded, then disappear
                   condition = TRUE,#"output.fileUploaded",
                   fileInput("file1", "Import a File",
                             accept = c(
                               "text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")
                   ) 
                 ),
                 downloadButton('downloadData', 'Download Data'),
                 downloadButton('downloadPlot', 'Download Plot')
               ),
               
               box(
                 title = "Plot Setting",
                 id = "mybox",
                 collapsible = TRUE,
                 closable = FALSE,

                 radioButtons(inputId='fileFormat', label='select the file type', choices=list('png','jpeg' ,'pdf')),
                 textInput('file_name:','file name','default'),
                 textInput('xlabel','x label', 'Time'),
                 textInput('ylabel', 'y label', 'Mean'),
                 textInput('title', 'title', 'Default Title')
               )
               
               
      ), 
      tabPanel("Table", DT::dataTableOutput("table"),downloadButton('save_t', 'Save')),
      #tabPanel("Summary", tableOutput("summary"),downloadButton('save_s', 'Save')),
      tabPanel("Settings",fluidRow(
          box(
          title = "Text Size",
          sliderInput("font_size", "Font Size (general):", min = 80, max = 110 , value = 100),
          sliderInput("label_size", "Label Size:", min = 0, max = 30 , value = 17),
          sliderInput("title_size", "Title Size:", min = 0, max = 30 , value = 20),
          sliderInput("axis_size", "Axis text Size:", min = 0, max = 30 , value = 15)
          ),
          box(
          title = "Plot Size",
          sliderInput("plot_width", "Plot Width:", min = 200, max =700 , value = 570),
          sliderInput("plot_height", "Plot height:", min = 50, max =500 , value = 350)
          )
      ))
    ),
    
    
  )#end of body
)

server <- function(input, output) {
  
  current <- reactiveVal(0)  # current number of treatments
  
  # uploading the file
  
  #previous version
  # data <- reactive({
  #   validate(
  #     need(input$file1 != "", "Please select a data set")
  #   )
  #   inFile <- input$file1
  #   if (is.null(inFile))
  #     return(NULL)
  #   df <- read.csv(inFile$datapath, header = TRUE,fileEncoding="UTF-8-BOM")
  #   df
  # })
  
  # get the data set
  data <- reactive({
    validate(
          need(input$file1 != "", "  Notice: Please select a data file !!")
    )
    if(is.null(input$file1)) {
      return(NULL)
    } else {
      return(read.csv(input$file1$datapath, header = TRUE))
    }
  })
  
  # Check if file has been uploaded:
  # if yes, then upload button disappear
  output$fileUploaded <- reactive({
    return(is.null(data()))
  })
  
  
  #observeEvent for graph selector TODO: Decide graph types and modify this sector
  observeEvent(c(input$graph,input$file1),{
    #initial page for graph selector
    if(input$graph=="Default"){
      #no other selector will show up
      output$loc<-renderUI({
      })
    }
    #parameter selectors for stacked graph
    else if(input$graph=="Stacked" | input$graph == "Stacked(percentage)"){
      output$loc<-renderUI({
        selectInput("segment_var", "Choose segment variables:",
                    choices = tail(colnames(data()),-3),multiple = TRUE)
      })
    }
    #parameter selectors for linear graph
    else if(input$graph=="Linear"){
      output$loc<-renderUI({
        selectInput("variables", "Choose variables:",
                    choices = tail(colnames(data()),-3),multiple = TRUE)
      })
    }
  })#end of observeEvent for graph selector
  
  #Check if we need to set up groups of treatment
  observeEvent(input$set,{
    output$treat<-renderUI({
      if(input$set==TRUE){
        current(0)
        list(
          actionButton("add","Add"),
          actionButton("delete","Delete")
        )
      }
    })
  })
  #Check if we need to set up range of timepoint
  observeEvent(input$time,{
    output$time_range<-renderUI({
      if(input$time==TRUE){
        max_num <- as.integer(tail(unique(data()[,3]),n=1))
        sliderInput("slider", label = h5(strong("Time Point")), min = 1, 
                    max = max_num, step=1,
                    value = c(1, max_num))#file depend
      }
    })
  })
  
}
library(stats)
library(tidyverse) # this includes dplyr for data wranling, ggplot2 for plotting, and tidyr for reshaping data
library(shiny)
library(plotrix) # for standard error function
library(shinythemes)
library(gridExtra)
library(colourpicker)
library(shinyjs) 
#library(DT)
df<-NA

# Function that produces default gg-colours is taken from this discussion:
# https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
gg_fill_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
ui <- NA
server <- NA
#shiny
#YL build structure
#if (interactive()) {
#build up ui
ui <- fluidPage(#theme = shinytheme("cerulean"),
  
  useShinyjs(),   # use js in shiny
  titlePanel("Death TrackR"),
  sidebarLayout(
    sidebarPanel(
      #file upload
      fileInput("file1", "Import CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
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
    ),#end of siderbar panel
    mainPanel(
      #tabset for plot, summary and table
      tabsetPanel(
        tabPanel("Plot", uiOutput('colors'),
                 plotOutput("plot",inline=TRUE),
                 downloadButton('downloadData', 'Download Data'),
                 downloadButton('downloadPlot', 'Download Plot'),
                 radioButtons(inputId='fileFormat', label='select the file type', choices=list('png','jpeg' ,'pdf')),
                 textInput('file_name:','file name','default'),
                 textInput('xlabel','x label', 'Time'),
                 textInput('ylabel', 'y label', 'Mean'),
                 textInput('title', 'title', 'Default Title')
                 
                 
        ), 
        tabPanel("Table", DT::dataTableOutput("table"),downloadButton('save_t', 'Save')),
        #tabPanel("Summary", tableOutput("summary"),downloadButton('save_s', 'Save')),
        tabPanel("Settings",fluidRow(
          themeSelector(),
          sliderInput("font_size", "Font Size (general):", min = 80, max = 110 , value = 100),
          sliderInput("label_size", "Label Size:", min = 0, max = 30 , value = 17),
          sliderInput("title_size", "Title Size:", min = 0, max = 30 , value = 20),
          sliderInput("axis_size", "Axis text Size:", min = 0, max = 30 , value = 15),
          sliderInput("plot_width", "Plot Width:", min = 200, max =700 , value = 570),
          sliderInput("plot_height", "Plot height:", min = 50, max =500 , value = 350)
        ))
      )
      
    )#end of main panel
  )
  
  
  
)#end of ui


#build up server
server <- function(input,output,session) {
  #function for uploading the file
  data <- reactive({
    validate(
      need(input$file1 != "", "Please select a data set")
    )
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE,fileEncoding="UTF-8-BOM")
    df
  })
  #observeEvent for graph selector 
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
  #add button
  observeEvent(input$add, {
    newValue <- current() + 1
    current(newValue)
    insertUI(
      selector = "#add",
      where = "beforeBegin",
      ui = selectInput(paste("group",current(),sep=""), paste("Treatment:",current()),
                       list('well name'=unique(data()[,1])),multiple = TRUE)
    )
  })
  #delete button
  observeEvent(input$delete, {
    ui_todelete <- sprintf(".shiny-input-container:has(#group%s)",current())
    removeUI(
      selector = ui_todelete
    )
    newValue <- current() - 1
    if (newValue < 0) {
      newValue <- 0
    }
    current(newValue)
  })
  
  # table tabpanel output
  output$table <- DT::renderDataTable({
    datatable()
  })
  
  #
  datatable <- reactive({
    #display dataframe
    inFile <- input$file1
    df <- read.csv(inFile$datapath,header=TRUE,fileEncoding="UTF-8-BOM")
    treat.names = all_treat()
    params = input$segment_var
    d=NULL
    i<-1
    for (treat in treat.names){
      #print(treat)
      df1_long<-sub.table.all(df,treat,params)
      d1<- df1_long %>%
        mutate(group = i) %>%
        filter(Time.Point>=input$slider[1],Time.Point<=input$slider[2]) %>%
        select(!smean) %>%
        select(group, everything())%>%
        arrange(Time.Point)
      d <- bind_rows(d,d1)
      #print("hello a")
      i<-i+1
    }
    d
  })
  
  #Get current number of treatments
  current <- reactiveVal(0)
  #Output all selected treatments(TEST!!!)
  output$summary <- renderTable({
    all_treat()
  })
  
  #automatically select Color
  output$colors <- renderUI({
    if(input$graph=="Stacked" | input$graph=="Stacked(percentage)"){
      lev <- sort(unique(input$segment_var))
    }
    else if(input$graph=="Linear"){
      lev <- sort(unique(input$variables))
    }
    else{
      lev <- c()
    }
    cols <- gg_fill_hue(length(lev))
    
    # New IDs "colX1" so that it partly coincide with input$select...
    lapply(seq_along(lev), function(i) {
      colourpicker::colourInput(inputId = paste0("col", lev[i]),
                                label = paste0("Choose color for ", lev[i]), 
                                value = cols[i]
      )        
    })
  })
  #Plot by choice
  observeEvent(input$plot_button,{
    params <- input$variables
    #1) Stacked Graph
    if(input$graph=='Stacked'){
      d <- dataBar()
      output$plot <- renderPlot({
        plotBar(d,params)
      }, 
      width=input$plot_width,
      height=input$plot_height+(current()-1)*(input$plot_height-input$plot_height/3),
      execOnResize = TRUE
      )
    }
    
    else if(input$graph=="Stacked(percentage)"){
      d <- dataBar()
      output$plot <- renderPlot({
        plotBar_percentage(d,params)
      }, 
      width=input$plot_width,
      height=input$plot_height+(current()-1)*(input$plot_height-input$plot_height/2),
      execOnResize = TRUE
      )
    }
    
    ## Linear Plot
    else if(input$graph=="Linear"){
      d <- dataLinear()
      
      output$plot <- renderPlot({
        ggplot(d,aes(x=Time.Point,y=mean))+
          geom_point(aes(color=key),position=position_dodge(0.3)) +
          geom_errorbar(
            aes(ymin = mean-se, ymax=mean+se, color="error bar"),
            position = position_dodge(0.3), width = 0.6
          )+
          facet_grid(group~.,scales="free")+
          labs (x=input$xlabel,y=input$ylabel,title=input$title)+
          scale_color_manual(values=all_colors())+
          theme_classic()+
          theme(strip.background = element_blank(), 
                strip.placement = "outside", legend.position = "bottom",
                plot.title = element_text(hjust = 0.5, size=input$title_size, face="bold"),
                axis.text=element_text(size=input$axis_size),
                axis.title=element_text(size=input$label_size)
          )

        
      },  
      width=input$plot_width,
      height=input$plot_height+(current()-1)*(input$plot_height-50),
      execOnResize = TRUE
      )
      
    }
  }
  
  )
  
  
  ### Put all helper methods down here ###
  
  #1.function for uploading the file
  data <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE,fileEncoding="UTF-8-BOM")
    df
  })
  
  # table for display
  sub.table.display <- reactive({
    inFile <- input$file1
    df <<- read.csv(inFile$datapath,header=TRUE,fileEncoding="UTF-8-BOM")
    treat.names = all_treat()
    params = input$segment_var
    
    df1.mean<-sub.table.mean(df,names,params)
    df1.se<-sub.table.se(df,names,params)
    
    df1_long.se <- df1.se %>%
      gather(key='key',value='se',factor_key = TRUE,-Time.Point)
    df1_long.mean <- df1.mean %>%
      gather(key='key',value='mean',factor_key = TRUE,-Time.Point)
    df1_long.smean <- df1.smean %>%
      gather(key='key',value='smean',factor_key = TRUE,-Time.Point)
    

    
  })
  
  # 1.5 make sure this is called after user has selected the treatment names etc.
  dataBar <- reactive({
    inFile <- input$file1
    df <<- read.csv(inFile$datapath,header=TRUE,fileEncoding="UTF-8-BOM")
    
    treat.names = all_treat()
    params = input$segment_var
    
    i<-1
    d = NULL
    for (treat in treat.names){
      df1_long<<-sub.table.all(df,treat,params)
      d1<- df1_long %>%
        mutate(group = i) %>%
        filter(Time.Point>=input$slider[1],Time.Point<=input$slider[2])
      d <- bind_rows(d,d1)
      i<-i+1
    }
    d
  })
  
  dataLinear <- reactive({
    params = input$variables
    treat.names = all_treat()
    
    i<-1
    d = NULL
    for (treat in treat.names){
      
      df1_long<<-sub.table.all(df,treat,params)
      d1<- df1_long %>%
        mutate(group = i) %>%
        filter(Time.Point>=input$slider[1],Time.Point<=input$slider[2])
      d <- bind_rows(d,d1)
      i<-i+1
    }
    d
  })
  # Plot the sacked bar plot
  plotBar <- function(d,params){
    #d$key <- factor(d$key, levels=params)
    gg<-ggplot(d,aes(x=Time.Point,y=mean, fill=key))+
      geom_bar(stat = "identity") +
      geom_errorbar(
        aes(ymin = smean-se, ymax=smean+se),
        position = "dodge", width = 1,
        colour="black"
      )+
      ggtitle(input$title)+
      facet_grid(group~.,scales="free_y", shrink=TRUE)+
      labs (x=input$xlabel,y=input$ylabel)+
      scale_fill_manual(values = all_colors())+
      theme_classic()+
      theme(strip.background = element_blank(), 
            strip.placement = "outside", legend.position ="bottom", panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), strip.text.y=element_blank(),
            plot.title = element_text(hjust = 0.5, size=input$title_size, face="bold"), 
            axis.text=element_text(size=input$axis_size),
            axis.title=element_text(size=input$label_size))
    
    gg
  }
  # Stacked bar plot with vertical axis percentage
  plotBar_percentage <- function(d,params){
    #d$key <- factor(d$key, levels=params)
    gg<-ggplot(d,aes(x=Time.Point,y=mean, fill=key))+
      geom_bar(stat = "identity", position="fill") +
      ggtitle(input$title)+
      facet_grid(group~.,scales="free_y", shrink=TRUE)+
      labs (x=input$xlabel,y=ylabel())+
      scale_fill_manual(values = all_colors())+
      theme_classic()+
      theme(strip.background = element_blank(), 
            strip.placement = "outside", legend.position ="bottom", panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), strip.text.y=element_blank(),
            plot.title = element_text(hjust = 0.5, size=input$title_size, face="bold"), 
            axis.text=element_text(size=input$axis_size),
            axis.title=element_text(size=input$label_size))
    
    gg
  }
  
  # 2.take sub-table by selected 'Well Name' and Params(cols). Add mean and std error for each columns var.
  # In current version, we're using #6, which returns a enlongated(see function 'gather') version of data frame
  # msg me if any confusion (Ricky)
  sub.table <- function(df,names, params){
    #inFile <- input$file1
    #if (is.null(inFile))
    #  print("There's no input file")
    #  return(NULL)
    params1<-append(params,"Time.Point",after=0)
    df1 <- df %>%
      filter('Well.Name' %in% names) %>%
      select(params1) %>%
      group_by(Time.Point) %>%
      summarise_all(list(~mean(.),~std.error(.)))
    
    df1
  }
  
  # 3. return names of all treatments as list of vectors
  all_treat <- function(){
    result <- list(current())
    for (i in seq(current())) {
      treat<-c(input[[paste0("group",as.character(i))]])
      result[[i]] <- treat
    }
    
    result
  }
  
  # 4. return sub table with mean, called in #6
  sub.table.mean <- function(df,names, params){
    params1<-append(params,c("Time.Point"),after=0)
    df1<- df %>%
      filter(Well.Name %in% unlist(names)) %>%
      select(params1) %>%
      group_by(Time.Point) %>%
      summarise_all(list(~mean(.)))
    #for (i in 2:ncol(df1)){df1[,i]=df1[,i-1]+df1[,i]}

    df1
    
  }
  # 4.5. return sub table with stacked mean, called in #6
  sub.table.smean <- function(df,names, params){
    params1<-append(params,c("Time.Point"),after=0)
    df1<- df %>%
      filter(Well.Name %in% unlist(names)) %>%
      select(params1) %>%
      group_by(Time.Point) %>%
      summarise_all(list(~mean(.)))
    if (ncol(df1)>2){
      for (i in (ncol(df1)-1):2){df1[,i]=df1[,i+1]+df1[,i]}
    }
    df1
    
  }
  
  # 5. return sub table with se, called in #6
  sub.table.se <- function(df,names, params){
    params1<-append(params,c("Time.Point"),after=0)
    df1<- df %>%
      filter(Well.Name %in% unlist(names)) %>%
      select(params1) %>%
      group_by(Time.Point) %>%
      summarise_all(list(~std.error(.)))
    
    df1
  }
  
  # 6. return a long format data frame with mean and se (can add other statistics later)
  # I recommend run local first to see how the returned table looks like
  sub.table.all <- function(df, names, params){
    #params <-  params[order(nchar(params),params)]
    # call #4 and #5, see above
    df1.mean<-sub.table.mean(df,names,params)
    df1.se<-sub.table.se(df,names,params)
    df1.smean <-sub.table.smean(df, names, params)
    
    df1_long.se <- df1.se %>%
      gather(key='key',value='se',factor_key = TRUE,-Time.Point)
    df1_long.mean <- df1.mean %>%
      gather(key='key',value='mean',factor_key = TRUE,-Time.Point)
    df1_long.smean <- df1.smean %>%
      gather(key='key',value='smean',factor_key = TRUE,-Time.Point)
    
    df1_long <- merge(merge(df1_long.mean,df1_long.se),df1_long.smean)
    df1_long
  }
  
  #6. generate default download file name
  download.name <- reactive({
    treats <- all_treat()
    name <-""
    for (i in 1:length(treats)){
      a <- unlist(treats[i])
      for (j in 1:length(a)) {
        if (j!=1){
          name <- paste(name, a[j],sep='-')
        }else{
          name <- paste(name, a[j], sep='')
        }
      }
      name<-paste(name,'#',sep="")
    }
    substr(name,1, nchar(name)-1)
  })
  
  #7.Collect all colors inputs
  all_colors <- reactive({
    result <- list()
    lev <-NA
    if(input$graph=="Stacked" | input$graph=="Stacked(percentage)"){

      lev <- unique(input$segment_var)
    }
    else if(input$graph=="Linear"){
      lev <- unique(input$variables)
    }
    else{
      lev <- c()
    }
    for(i in lev){
      #collect treatment wells for each group
      color<-c(input[[paste0("col", i)]])
      result <- c(result, color)
    }
    
    if(input$graph=="Linear"){
      result <- c(result, "black")
      
    }
    result
    
  })
  
  # Select the theme for the app (Shiny inbuilt)
  themeSelector <- function() {
    div(
      div(
        selectInput("shinytheme-selector", "Choose a theme",
                    c("default", shinythemes:::allThemes()),
                    selectize = FALSE
        )
      ),
      tags$script(
        "$('#shinytheme-selector')
        .on('change', function(el) {
        var allThemes = $(this).find('option').map(function() {
        if ($(this).val() === 'default')
        return 'bootstrap';
        else
        return $(this).val();
        });
        // Find the current theme
        var curTheme = el.target.value;
        if (curTheme === 'default') {
        curTheme = 'bootstrap';
        curThemePath = 'shared/bootstrap/css/bootstrap.min.css';
        } else {
        curThemePath = 'shinythemes/css/' + curTheme + '.min.css';
        }
        // Find the <link> element with that has the bootstrap.css
        var $link = $('link').filter(function() {
        var theme = $(this).attr('href');
        theme = theme.replace(/^.*\\//, '').replace(/(\\.min)?\\.css$/, '');
        return $.inArray(theme, allThemes) !== -1;
        });
        // Set it to the correct path
        $link.attr('href', curThemePath);
        });"
        )
      )
}
  
  # Change font size. Listening on the slider under settings tab
  observeEvent(input$font_size, {
    runjs(paste0('$("*").css("font-size","', input$font_size, '%")'))
  })
  
  # download button
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$df, '.csv', sep='') },
    content = function(file) {
      write.csv(datatasetInput(), file)
    }
  )
  
  fname <- reactive({
    f<-input$file_name
    if(input$file_name=="default"){
      f<-download.name()
    }
    f
  })
  
  ylabel <- reactive({
    result <-"Mean"
    if(input$graph=="Stacked(percentage)"){
      result<-"Percentage(%)"
    }
    if(input$ylabel!="Mean" & input$ylabel!="Percentage(%)"){
      result <- input$ylabel
    }
    result
  })
  
  output$downloadPlot <- downloadHandler(

    filename = function() { paste(fname(), input$fileFormat, sep='.') },
    content = function(file) {
      if(input$fileFormat == 'png'){
        png(file,
            width=input$plot_width,
            height=input$plot_height+(current()-1)*(input$plot_height-input$plot_height/2)
            )
      }
      else if(input$fileFormat == 'jpeg'){
        jpeg(file,
            width=input$plot_width,
            height=input$plot_height+(current()-1)*(input$plot_height-input$plot_height/2)
        )
      }
      else{
        # pointsize to be approximately in points
        # default scale is inch
        pdf(file,
            width=input$plot_width/72,
            height=(input$plot_height+(current()-1)*(input$plot_height-input$plot_height/2))/72,
            pointsize=12
        )
      }
      d <- dataBar()
      g<-NA
      if (input$graph=="Stacked"){
        g<-plotBar(d)
      } else if(input$graph=="Stacked(percentage)"){
        g<-plotBar_percentage(d)
      } else{
        g<-plotBar(d)
      }
      output$plot <- renderPlot({ g }, 
                                width=input$plot_width,
                                height=input$plot_height+(current()-1)*(input$plot_height-input$plot_height/2)
                                )
      print(g)
      dev.off()
    }
  )

  output$save_t <- downloadHandler(
    filename = function() {
      paste(fname(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datatable(), file, row.names = TRUE)
    }
  )
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(fname(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datatable(), file, row.names = TRUE)
    }
  )
  
  
  }#end of server  
#}end of check the interactive
shinyApp(ui, server)


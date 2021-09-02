## server.R ##
library(readxl)
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(data.table)
library(colourpicker) # install.packages("colourpicker")
library(devtools)
library(gridExtra)
library(grid)
library(janitor)
library(dplyr)
library(tidyverse)
library(plotly)
library(tidyr)
library(plyr)
library(formattable)



not_sel <- "Not Selected"

themes <- list("Light" = theme_light(),
               "Dark" = theme_dark(),
               "Gray" = theme_gray(),
               "Minimal" = theme_minimal(),
               "Classic" = theme_classic())


#By defaults the file size limit is 5MB, It can be changed by setting this option. Here we will raise it limit to 20mb.
options(shiny.maxRequestSize = 30*1024^2)

# Define server logic ----
server <- function(input, output, session) {
  
  ############################################################
  ############## Datasets in and out #########################
  ############################################################
  
  # loading data
  data <- eventReactive(input$file, {
    extension <- tools::file_ext(input$file$name)
    filepath <- input$file$datapath
    # file1 <- input$file$name
    if(extension == "csv"){
      read.csv(filepath)
    }else if (extension == "xls"){
      readxl::read_xls(filepath, .name_repair = "universal")
    }else if (extension == "xlsx"){
      read_excel(filepath ,
                 .name_repair = "universal")
      #file1 <- janitor::clean_names(file1)
    }
  })
  
  
  #get data from data browser
  data_input <- reactive({
    data()
  })
  
  
  # name of the file
  
  
  # info of the file
  output$about_file <- renderPrint({
    inFileName <- input$file$name
    tmp <- data()
    nm1 <- sapply(tmp, class)
    nm2 <- sapply(tmp, function(x) length(unique(x)))
    cat(paste(inFileName,"(", nrow(tmp),
              'rows ', ncol(tmp), ' columns'),"):", "\n", "\n")
    cat(paste(names(nm1),":" , nm1, "( Unique Value :",nm2, ")", collapse = "\n"), "\n") 
  })
  
  # to display data
  output$display <- renderDataTable({
    dataset = data()},
    filter = 'top',escape = FALSE, options = list(pageLength = 10, 
                                                  scrollX='500px',autoWidth = TRUE))
  
  # passing to UI finally
  output$data_browsing<-renderUI({
    if(is.null(data())){
      
    }else{
      tabsetPanel(tabPanel("Data Browse",dataTableOutput("display")),
                  tabPanel("Data Structure", verbatimTextOutput("about_file")))
    }})
  # End of Loading data
  
  ############################################################
  #################  End of datasets part ####################
  ############################################################
  
  
  ############################################################
  #############  Start of transformation part ################
  ############################################################
  
  IsDate <- function(mydate, date.format = "%Y-%m-%d") {
    tryCatch(!is.na(as.Date(mydate, date.format)),  
             error = function(err) {FALSE})  
  }
  
  
  myfunc <- function(){reactive({
    l <- list()
    for(i in 1:ncol(data())) {            
      l[[i]] <- names(data())[i]
    }
    return(l)})
  }
  
  colTrack_list <- myfunc()
  col_reactive <- reactiveValues()
  col_reactive$list <- reactive({colTrack_list()})
  
  
  # observe event for updating the reactiveValues
  observeEvent(input$applySelect,
               {
                 df <- data()
                 colnames(df) <- col_reactive$list() #assign new column names to original data after rename action occurred
                 ndf <- newdata_reactive$newdata()
                 
                 if(length(input$colToSelect) > 0){
                   ndf <- df[c(input$colToSelect)]
                   newdata_reactive$newdata <- reactive({ndf})
                 }
               }
  )
  
  
  observeEvent(input$applyRename,
               {
                 ndf <- newdata_reactive$newdata()
                 names(ndf)[names(ndf) == input$colToRename] <- input$newColName
                 newdata_reactive$newdata <- reactive({ndf})
                 
                 x <- col_reactive$list()
                 x <- replace(x,match(c(input$colToRename),x),input$newColName)
                 col_reactive$list <- reactive({x})
               }
  )
  
  
  observeEvent(input$applyType,
               {
                 ndf <- newdata_reactive$newdata()
                 s <- input$colToChangeType
                 s <- sub("\\(.*", "", s)
                 mycol <-c(s)
                 
                 if(input$newColDatatype == 1){ #character
                   for (i in mycol)ndf[,i]<-as.character(ndf[,i])
                 }
                 else if(input$newColDatatype == 2){ #numeric
                   for (i in mycol)ndf[,i]<-as.numeric(ndf[,i])
                 }
                 else if(input$newColDatatype == 3){  #factor
                   for (i in mycol)ndf[,i]<-as.factor(ndf[,i])
                 }
                 else if(input$newColDatatype == 4){  #Date
                   for (i in mycol)
                     ndf[,i]<-format(as.Date(ndf[,i],"%Y-%m-%d"),"%d/%m/%Y")
                 }
                 else if(input$newColDatatype == 5){  #logical
                   for (i in mycol)ndf[,i]<-as.logical(ndf[,i])
                 }
                 else{  #percentage
                   for (i in mycol)ndf[,i]<-as.character(percent(ndf[,i]))
                 }
                 newdata_reactive$newdata <- reactive({ndf})
               }
  )
  
  
  observeEvent(input$applyCheckButton,{
    if(input$applyCheckButton == 0) return(NULL) 
    else if (input$applyCheckButton%%2 == 0)
    {
      updateCheckboxGroupInput(session,"colToSelect","Check column(s) to select - at least one (1) column:",choices=as.list(col_reactive$list()),selected=as.list(col_reactive$list()))
    }
    else
    {
      updateCheckboxGroupInput(session,"colToSelect","Check column(s) to select - at least one (1) column:",choices=as.list(col_reactive$list()))
      
    }
  })
  
  
  
  newdata_reactive <- reactiveValues()
  newdata_reactive$newdata <- reactive({data()})
  
  
  output$transformedData <- renderDataTable({
    newdata_reactive$newdata()
  }, options = list(scrollX = TRUE))
  
  
  output$transformForm <- renderUI({
    
    action <- input$transformAction
    
    col_list <- list()
    colType_list <- list()
    
    # Using for-loop to add columns to list
    for(i in 1:ncol(newdata_reactive$newdata())) {            
      col_list[[i]] <- names(newdata_reactive$newdata())[i]
    }
    
    for(i in 1:ncol(newdata_reactive$newdata())) {
      colType_list[[i]] <- paste0(names(newdata_reactive$newdata())[i],"(",class(newdata_reactive$newdata()[,names(newdata_reactive$newdata())[i]]),")")
    }
    
    
    if(action == 'rename'){
      selectInput("colToRename",label = "Select column to rename:", as.list(col_list))
      
    } else if(action == 'select'){
      checkboxGroupInput("colToSelect", label = "Check column(s) to select - at least one (1) column:", as.list(col_reactive$list()), selected = as.list(col_list))
      
    } else {
      selectInput("colToChangeType",label = "Select column to change datatype/format:", as.list(colType_list))
    }
    
  })
  
  
  output$transformSubForm <- renderUI({
    action <- input$transformAction
    s <- sub(".*(?:\\((.*)\\)).*|.*", "\\1",input$colToChangeType)
    ndf <- newdata_reactive$newdata()
    t <- input$colToChangeType
    t <- sub("\\(.*", "",t)
    mycol <-c(t)
    
    if(action == 'rename'){
      textInput("newColName", label = "Enter new name:")
    }
    
    else if(action == 'select'){
      actionLink("applyCheckButton","Select/Deselect All")
    }
    
    else if(action == 'type'){
      
      if(s=="character"){
        
        my_list <- list() 
        for (i in mycol){
          my_list[[i]] <- IsDate(ndf[,i])
        }
        
        
        if((my_list[[1]][1]) == TRUE & length(unique(my_list))==1){
          radioButtons("newColDatatype", strong("Select new datatype/format:"),
                       choices = list("numeric" = 2, "factor" = 3, "Date" = 4, "logical" = 5)
          )
        }
        
        else {
          radioButtons("newColDatatype", strong("Select new datatype/format:"),
                       choices = list("numeric" = 2, "factor" = 3, "logical" = 5)
          )
        }
      }
      
      else if(s=="numeric"){
        radioButtons("newColDatatype", strong("Select new datatype/format:"),
                     choices = list("character" = 1, "factor" = 3, "logical" = 5, "percentage" = 6)
        )
      }
      
      else if(s=="factor"){
        
        my_list <- list() 
        for (i in mycol){
          my_list[[i]] <- IsDate(ndf[,i])
        }
        
        my_list1 <- list() 
        for (i in mycol){
          my_list1[[i]] <- IsDate1(ndf[,i])
        }
        
        
        if((my_list[[1]][1]) == TRUE & length(unique(my_list))==1){
          radioButtons("newColDatatype", strong("Select new datatype/format:"),
                       choices = list("character" = 1, "numeric" = 2, "Date" = 4, "logical" = 5)
          )
        }
        
        else if((my_list1[[1]][1]) == TRUE & length(unique(my_list1))==1){
          radioButtons("newColDatatype", strong("Select new datatype/format:"),
                       choices = list("character" = 1, "numeric" = 2, "Date" = 4, "logical" = 5)
          )
        }
        
        else {
          radioButtons("newColDatatype", strong("Select new datatype/format:"),
                       choices = list("character" = 1, "numeric" = 2, "logical" = 5)
          )
        }
      }
      
      else if(s=="Date"){
        radioButtons("newColDatatype", strong("Select new datatype/format:"),
                     choices = list("character" = 1, "numeric" = 2, "factor" = 3, "logical" = 5)
        )
      }
      
      else if(s=="logical"){
        radioButtons("newColDatatype", strong("Select new datatype/format:"),
                     choices = list("character" = 1, "numeric" = 2, "factor" = 3)
        )
      }
    }
  })
  
  
  output$apply <- renderUI({
    
    action <- input$transformAction
    
    if(action == 'rename'){
      actionButton("applyRename", label = "Apply")
    }    
    else if(action == 'select'){
      actionButton("applySelect", label = "Apply")
    } 
    else {
      actionButton("applyType"  , label = "Apply")
    }
  })
  
  
  ############################################################
  ##############  End of transformation part #################
  ############################################################
  
  
  #Visualization
  
  observeEvent(newdata_reactive$newdata(),{
    choices <- c(not_sel,names(newdata_reactive$newdata()))
    updateSelectInput(session, inputId = "num_var_1", choices = choices)
    updateSelectInput(session, inputId = "num_var_2", choices = choices)
    updateSelectInput(session, inputId = "fact_var", choices = choices)
  })
  
  num_var_1 <- eventReactive(input$run_button,input$num_var_1)
  num_var_2 <- eventReactive(input$run_button,input$num_var_2)
  fact_var <- eventReactive(input$run_button,input$fact_var)
  
  
  
  #End of uploading file---------------------------------------------------------------------------
  
  #Plot
  f2 <- list(
    family = "Old Standard TT, serif",
    size = 14,
    color = "black"
  )
  labelInput2 <- list(
    showticklabels = TRUE,
    tickangle = 45,
    tickfont = f2
  )
  
  labelInput <- reactive({
    if(input$angle == "90"){
      scale_x_discrete(guide = guide_axis(angle = 90))
    }
    else{
      scale_x_discrete(guide = guide_axis(angle = 45))
    }
  })
  
  #bar chart
  plot1 <- reactive({
    ggplot(newdata_reactive$newdata(), aes_string(x= num_var_1(),y=num_var_2(), fill = fact_var())) +
      xlab(input$num_var_1) +
      ylab(input$num_var_2)+
      #labelInput() +
      #theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      if(input$func == "Average"){
        geom_bar(position="stack", stat="summary",fun=mean) #, fill=input$colour_r
      }
      else if(input$func == "Sum"){
        geom_bar(position="stack", stat="summary",fun=sum) #, fill=input$colour_r
      }
      else{
        geom_bar(position="stack", stat="identity") #, fill=input$colour_r
      } 
      
      #theme(axis.text.x = element_text(size = 12)) +
      #theme(axis.text.y = element_text(size = 13)) +
      #layout(xaxis = labelInput())+
      #layout(legend = list(x = 1, y = 0.5)) #orientation = "h", 
      
    #stat_summary(geom = "text", colour = "black", size = 3.5,
    #aes(label = summary),position=position_stack(vjust=0.5))
    
    #if(input$fact_var == date){
    # scale_x_date(date_labels = "%m/%y")
    #}
    #as.Date(input$fact_var == "Date", format = "%m/%y")
    #scale_x_date(date_labels = "%b/%y") 
    #->Warning: Error in : Invalid input: date_trans works with objects of class Date only
  })
  
  
  #line chart
  plot2 <- reactive({
    ggplot(newdata_reactive$newdata(), aes_string(x=num_var_1(),y=num_var_2(),group=fact_var())) + # alpha=fact_var(),color=input$colour_r
      #labelInput()+
      theme(legend.position = "top") +
      if(input$func == "Average"){
        geom_line(aes_string(color=fact_var()),stat="summary", fun=mean, size=1) #aes_string(linetype=fact_var()),
      }
    else if(input$func == "Sum"){
      geom_line(aes_string(color=fact_var()),stat="summary", fun=sum, size=1) 
    }
    else{
      geom_line(aes_string(color=fact_var()),stat="identity", size=1) 
    }
  })
  
  #pie chart
  #takes a while to display -> long time
  #plotly does not support coord_polar code
  plot3 <- reactive({
    ggplot(newdata_reactive$newdata(), aes(x= "", y = num_var_1(), fill= num_var_2())) + 
      geom_bar(aes_string(fill = num_var_2()), stat="identity", width = 1) + #, show.legend = FALSE
      coord_polar("y", start = 0) +
      ylab(input$num_var_1)
    #theme(legend.position = "none") #to remove colour indicator
  })
  
  #scatter plot
  plot4 <- reactive({
    ggplot(newdata_reactive$newdata(), aes_string(x = num_var_1(), y = num_var_2(), fill=fact_var()))+
      #labelInput()+
      if(input$func == "Average"){
        geom_point(aes_string(fill=fact_var()),stat="summary",fun=mean, shape = 21,size=3) # ,fill = input$colour_r
      }
    else if(input$func == "Sum"){
      geom_point(aes_string(fill=fact_var()),stat="summary",fun=sum, shape = 21,size=3)
    }
    else{
      geom_point(aes_string(fill=fact_var()),shape = 21, size=3)
    }
    #scale_x_discrete(guide = guide_axis(angle = 90))
  })
  
  #heatmap
  plot5 <- reactive({
    ggplot(newdata_reactive$newdata(), aes_string(x=num_var_1(),y=fact_var(), fill = num_var_2()))+
      geom_tile()+
      #scale_fill_distiller(palette = input$colour_r)+
      scale_fill_gradient(low = "lightblue", high = "darkblue")
      #labelInput()
  })
  
  #area chart
  plot6 <- reactive({
    ggplot(newdata_reactive$newdata(),aes_string(x=num_var_1(),y=num_var_2(), fill = fact_var(), group = fact_var()))+
      #labelInput()+
      if(input$func == "Average"){
        geom_area(aes_string(fill = fact_var()), stat="summary",fun=mean, position="stack")
      }
    else if(input$func == "Sum"){
      geom_area(aes_string(fill = fact_var()), stat="summary",fun=sum, position="stack")
    }
    else{
      geom_area(aes_string(fill = fact_var()), stat="identity", position="stack")
    }
    #scale_x_date(date_labels = "%m-%Y")
  })
  
  
  #return requested graph
  graphInput <- reactive({
    #funcInput()
    switch(input$rd,
           "Bar" = plot1(),
           "Line" = plot2(),
           "Pie" = plot3(),
           "Scatter" = plot4(),
           "Heatmap" = plot5(),
           "Area" = plot6())
  })
  
  
  plot_theme <- reactive({
    themes[input$theme]
  })
  
  output$plot <- renderPlotly({ 
    if(input$flip_box == TRUE){
      graphInput()  +
        coord_flip() +
        plot_theme()
        #layout(xaxis = labelInput)
    }
    else{
      graphInput() +
        plot_theme()+
        theme(axis.text.x = element_text(angle = 45))
        #layout(xaxis = labelInput)
    }
  }) #,height=600
  
  observeEvent(input$reset_input,{
    choices <- c(not_sel,names(newdata_reactive$newdata()))
    updateSelectInput(session, inputId = "num_var_1", choices = choices)
    updateSelectInput(session, inputId = "num_var_2", choices = choices)
    updateSelectInput(session, inputId = "fact_var", choices = choices)
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() { paste(num_var_1(),input$filetype, sep=".") },
    #default name of file is set as categorical variable
    content = function(file) {
      #last_plot() saves the last design customization user did
      if(input$filetype == "jpg")
        ggsave(file, plot = last_plot(), device = "jpg")
      else if(input$filetype == "pdf")
        ggsave(file, plot = last_plot(), device = "pdf")
      else
        ggsave(file, plot = last_plot(), device = "png")
    }
  )
  
  #end of visualization
  
  
  ############################################################
  ###############  Start of Data Statistics ##################
  ############################################################
  
  #create datatable for statistics
  statsDF <- data.frame(x=character(),
                        y=character(),
                        z=character(),
                        minimum=numeric(),
                        median=numeric(),
                        mean=numeric(),
                        maximum=numeric(),
                        sd=numeric(),
                        variance=numeric(),
                        stringsAsFactors=FALSE)
  
  
  #make statsDF datatable reactive
  statsDF_reactive <- reactiveValues()
  statsDF_reactive$df<- reactive({statsDF})
  
  
  #create form for categorical variable
  output$catVarForm <- renderUI({
    ndf <- newdata_reactive$newdata() 
    catVar_list <- list() #create empty list

    # Using for-loop to add columns to list
    for(i in 1:ncol(ndf)) {
      if(!is.numeric(ndf[,i])){ 
        catVar_list <- append(catVar_list,names(ndf)[i]) #add col into option catVar_list if it's not numerical
      }
    }

    selectInput("catVar",label = "Select categorical variable (x):", as.list(catVar_list)) #'catVar' stores user input of the categorical variable

  })
  
  
  #create form for categorical value
  output$catValForm <- renderUI({
    
    var <- input$catVar #user input 'catVar' would be used here
    ndf <- newdata_reactive$newdata()
    
    catVal_list <- levels(as.factor(ndf[,input$catVar])) #use function 'levels' to get a list of all distinct value in col 'catVar'
    
    selectInput("catVal",label = "Select categorical value (y):", as.list(catVal_list)) #'catVal' stores user input of the categorical value
    
  })
  
  
  #create form for numerical value
  output$numVarForm <- renderUI({
    
    ndf <- newdata_reactive$newdata() 
    numVar_list <- list() #create empty list
    
    # Using for-loop to add columns to list
    for(i in 1:ncol(ndf)) {
      if(is.numeric(ndf[,i])){
        numVar_list <- append(numVar_list,names(ndf)[i]) #add col into option catVar_list if it's numerical
      }
    }
    
    selectInput("numVar",label = "Select numerical variable (z):", as.list(numVar_list)) #'numVar' stores user input of the numerical variable
    
  })
  
  
  #create button to add stats
  output$addStatsDF <- renderUI({

    actionButton("addStats"  , label = "Add")  #action button to add new stats
    
  })
  
  
  #create observeevent on action button 'addStats'
  observeEvent(input$addStats,
               {
                 
                 df <- statsDF_reactive$df() #this is stats datatable
                 
                 ndf <- newdata_reactive$newdata() #this is original data
                 
                 subndf <- subset(ndf, ndf[,input$catVar] == input$catVal) #this is a subset of the ori dataframe: subset ndf's rows (select only rows that contains the 'catVal' chosen by user)
                 inputList <- as.vector(subndf[,input$numVar]) #from the dataframe subset, create a vector of the 'numVar' column
                 
                 outputList <- c( input$catVar , input$catVal , input$numVar , min(inputList, na.rm = TRUE) , median(inputList, na.rm = TRUE), round(mean(inputList, na.rm = TRUE),2) , max(inputList, na.rm = TRUE), round(sd(inputList, na.rm = TRUE), 2), round(var(inputList, na.rm = TRUE),2) ) #this list has all values for one row in stats datatable
                 df <- rbind(df , outputList) #add above list into stats datatable 
                  
                 colnames(df) <- c("x","y","z","minimum","median","mean","maximum", "sd", "variance") #col naming must be done bcs the datatable is reactive
                 df[,"minimum"]<-as.numeric(df[,"minimum"])
                 df[,"median"]<-as.numeric(df[,"median"])
                 df[,"mean"]<-as.numeric(df[,"mean"])
                 df[,"maximum"]<-as.numeric(df[,"maximum"])
                 df[,"sd"]<-as.numeric(df[,"sd"])
                 df[,"variance"]<-as.numeric(df[,"variance"])
                 
                 statsDF_reactive$df <- reactive({df}) #assign stats datatable into reactive stats datatable
        
               }
  )
  
  #display the reactive stats datatable
  output$statsDataframe <- renderDataTable({
    statsDF_reactive$df()
  }, extensions = 'Buttons',  options = list(scrollX = TRUE, 
                                             dom = 'Bfrtip',
                                             buttons = list(
                                               #list(extend = 'copy', title = sub('\\.csv$', '', input$file$name)), 
                                               list(extend = 'csv', title = sub('\\.csv$', '', input$file$name)), 
                                               list(extend = 'excel', title = sub('\\.csv$', '', input$file$name)), 
                                               list(extend = 'pdf', title = sub('\\.csv$', '', input$file$name))
                                               #list(extend = 'print', title = sub('\\.csv$', '', input$file$name)) 
                                             )))
  
  
  #create observeevent on action button 'delStats'
  observeEvent(input$delStats,
               {
                 
                 df <- statsDF_reactive$df() #this is stats datatable
                 
                 if(input$rowNum > 0) {
                   df <- df %>% slice(-c(input$rowNum))#delete selected rowNum
                 }
                 
                 statsDF_reactive$df <- reactive({df}) #assign stats datatable into reactive stats datatable
                 
               }
  )
  
  
  
  
  ############################################################
  ################  End of Data Statistics ###################
  ############################################################
}


# Run the app ---- not needed because not compiled in one application
#shinyApp(ui ,server)
## ui.R ##
library(shinydashboard)
library(readxl)
library(DT)
library(ggplot2)
library(colourpicker)
library(devtools)
library(plotly)
library(tidyr)
library(plyr)
library(ggthemr)
library(formattable)


not_sel <- "Not Selected"

themes <- list("Light" = theme_light(),
               "Dark" = theme_dark(),
               "Gray" = theme_gray(),
               "Minimal" = theme_minimal(),
               "Classic" = theme_classic())


ui <- tags$head(
  HTML(
    "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
  )
)
dashboardPage(
  dashboardHeader(title = "SWAT Analytics Basic"),
  
  #Sidebar Content
  dashboardSidebar(
    sidebarMenu(
      br(),
      menuItem("About this Dashboard", tabName = "welcome", icon = icon("book-open")),
      menuItem("Data Browser", tabName = "browse", icon = icon("table")),
      menuItem("Data Transformation", tabName = "transform", icon = icon("exchange-alt")),
      menuItem("Data Visualization", tabName = "visualize", icon = icon("chart-bar")),
      menuItem("Data Statistics", tabName = "statistics", icon = icon("glyphicon glyphicon-th-list", lib = "glyphicon")),
      menuItem("Help", tabName = "help", icon = icon("question"))
      
    )
  ),
  
  dashboardBody(
    
    # css idk how to use actually (rest of the header) 
    tags$head(tags$style(HTML('
   
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #040050;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #040050;
        }
                              
        .skin-blue .main-header .navbar {
                              background-color: #040050;
        }   
                            

       /* .box.box-solid.box-primary{

                background:#ffa500;
        } */
        
      
      /*  body {color:white;} */
      
      .skin-blue .main-sidebar{
        background-color: #ffa500;
      }
      
      .skin-blue .main-sidebar .sidebar .sidebar-menu a{
        color: #000000;
      }
      
      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
        background-color: #040050;
        color: #ffffff;
      }
      
      .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
        background-color: #040050;
        color: #ffffff;
      }
      
    '))),
    
    tabItems(
      #First tab item content
      tabItem(
        tabName = "welcome",
          fluidRow(
            #box(
                width = 12,
                column(6, div(img(src = "image-1.png", width = "80%"), style = "text-align:center; margin-top: 15px;") ),
                column(6, 
                       br(),
                       h3(p("Welcome to the SWAT Analytics Basic Dashboard")),
                       br(),
                       h4(p("The functionality available:")),
                       HTML('<p><ul><li>Data Browser</li><li>Data Transformation</li><li>Data Visualization</li><li>Data Statistics</li></ul></p>'),
                       br(),
                      # h5(p("Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.")),
                       br(),
                       h4("This dashboard was built with RShiny by RStudio",
                          img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "25px"),
                          #"  by  ",
                          img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "25px"))
                       )
                
                #) #end of box
          ),
        
        
      ), #end of first tab item
    
      tabItem(
        #Tab for second item content
        tabName = "browse",
        fluidPage(
          
          titlePanel("Data Browser"),
          sidebarLayout(
            
            box(width = 12,
                br(),
                # radioButtons("filetype", "Select file type",choices=c("csv file","xlsx file", "xls file")),
                fileInput("file","Choose file to upload..",accept = c("text/csv","text/comma-separated-values,text/plain",".csv",".xlsx",".xls"))
                
            ),
            mainPanel(
              width = 12,
              uiOutput("data_browsing")
            )
          )
          
        ) #end of fluid row #2
      ),# end of second tab item
    
    
    tabItem(
      #Tab for third item content
      tabName = "transform",
      fluidPage(
        titlePanel("Data Transformation"),
        fluidRow(
          box(strong("Choose action.."),
              width = 3,
              selectInput("transformAction",(""),
                          choices = c("Select column" = "select",
                                      "Rename column" = "rename",
                                      "Change datatype" = "type"),
                          selected = "select")
          ),
          box(#strong("Perform action.."),
              width = 9,
              uiOutput("transformForm"),
              uiOutput("transformSubForm"),
              uiOutput("apply")
          )
        ),
        fluidRow(
          box(title = "Transformed data",
              width = 12,
              DTOutput(outputId = "transformedData")
          )
        )
      )#end of fluid row #3
    ),# end of third tab item

    tabItem(
      #Tab for fourth item content
      tabName = "visualize",
      fluidPage(
        titlePanel("Data Visualization"),
        
        fluidRow(
          
          box(
            solidHeader = TRUE,
            width = 12,
            status = "primary",
            
            main_page <- tabPanel(
              title = "Analysis",
              sidebarLayout(
                sidebarPanel(
                  title = "Inputs",
                  #fileInput("csv_input", "Select CSV File to Import", accept = ".csv"),
                  #checkboxGroupInput("rd","Select Chart:", choices = c("Bar","Line","Pie", "Scatter")),
                  selectInput("rd","Select Chart:", choices = c("Bar","Line", "Scatter", "Area", "Heatmap")), #"Pie",
                  selectInput("num_var_1", "Variable 1:", choices = c(not_sel)),
                  selectInput("num_var_2", "Variable 2:", choices = c(not_sel)),
                  selectInput("fact_var", "Categorical Variable:", choices = c(not_sel)),
                  #radioButtons("angle", "Label Angle:", choices = c("45","90"), selected = "45"),
                  selectInput("func", "Function", choices = c("Average","Count","Sum"),selected = "Sum"),
                  br(),
                  actionButton("run_button", "Run Analysis", icon = icon("play")),
                  actionButton("reset_input", "Reset"),
                  checkboxInput("flip_box", "Flip", value = FALSE),
                  br(),
                  selectInput("theme","Plot theme:", choices = names(themes)),
                  #selectInput("colourtheme","Colour theme:", choices = names(colourthemes)),
                  #colourInput("colour_r", "Fill colour:","lightblue", showColour = "background"),
                  #selectInput("filetype","Download type:", c("jpg","png","pdf")),
                  #downloadButton("downloadPlot","Download", icon = icon("download"))
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel(
                      title = "Plot",
                      plotlyOutput("plot", height = 600)
                    )
                  )
                )
                
                
                
                
              )
            )
          ),
        ) #end of fluid row #4
      )# end of fluid page
    ), # end of fourth tab item
    
    tabItem(
      #Tab for fifth item content
      tabName = "statistics",
      fluidPage(
        titlePanel("Data Statistics"),
        fluidRow(
          box(
            width = 4,
            uiOutput("catVarForm"),
            uiOutput("catValForm"),
            uiOutput("numVarForm"),
            uiOutput("addStatsDF")
          ),
          box(title = "Statistical Summary Table",
              width = 8,
              column(width=2, numericInput("rowNum", label="Delete row:", value=1), style = "display:inline-block; padding:0px; margin-bottom:20px;"),
              column(width=1, actionButton("delStats", label = NULL , icon = icon('trash')), style = "display:inline-block; padding-top:25px;"),
              DTOutput(outputId = "statsDataframe")
          )
        )
      )#end of fluid row #5
    ),# end of fifth tab item
    
    tabItem(
      #Tab for sixth item content
      tabName = "help",
      titlePanel("Help"),
      
      fluidRow(
        box(
          solidHeader = TRUE,
          width = 12,
          status = "primary",
          
          
          column(6, div(img(src = "welcome.gif", width = "75%"), style = "text-align:center; margin-top: 30px;") ),
          
          #h4(p(actionLink("link_to_dashboard", "About The Dashboard"))),
          #h5(p("The manuals on how to use the dashboard can be downloaded below. Please click on the download button to get the guide on how to use the SWAT Analytics Basic Dashboard.s")),
          
          h4(p(actionLink("link_to_browser", "Data Browser"))),
          h5(p("The Data Browser tab provide the functionality to upload the data file into the dashboard before the data being transform or visualize.")),
          
          h4(p(actionLink("link_to_transformation", "Data Transformation"))),
          h5(p("The Data Transformation tab allows the user to tranform the data based on the functionality available.")),
          
          h4(p(actionLink("link_to_visualization", "Data Visualization"))),
          h5(p("The Data Visualization is for the user to visualize the data based on the variables available in the datasets.")),
          
          a(href="test.pptx", h5(p("Download Manual")), download=NA, target="_blank"),
          #HTML('<p>For more information on the Dashboard, please visit the link below:<br/> <a href = "https://shiny.rstudio.com/reference/shiny/" >RShiny Dashboard by RStudio</a></p>')
          
        )
      ) #end of fluid row #6
    )# end of sixth tab item
    
  ) # ENd of tabItems (must include inside this for tab item)
    
    
  ) #end of dashboard body
)
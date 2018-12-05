#
##
###DataOneTwo web app
###Ingests CSV data files, cleans and predicts data
###VERSION 1.0.
###December, 2018
###See license.md for MIT License
###Isayev, Alex (main contributor)
##
#

##Install necessary packages
##Project is package version agnostic as of VERSION 1.0.

#install.packages("shiny")
#install.packages("shinythemes")
#install.packages("dplyr")
#install.packages("DT")
#install.packages("ggplot2")
#install.packages("shinyjs")
#install.packages("Hmisc")

##Load installed libraries

library(shiny)
library(shinythemes)
library(shinyjs)
library(dplyr)
library(DT)
library(ggplot2)
library(Hmisc)

jscode <- "shinyjs.refresh = function() { location.reload(); }"

##UI design
ui <- fluidPage(
  
  #CSS theme
  theme = shinytheme("united"),
  
  # Title for page
  titlePanel("DataOneTwo"),
  
  #sidebar, complete layout
  sidebarLayout(
    
    #sidebar, inputs
    #these elements remain throughout page interactions/experience
    sidebarPanel(
      
      helpText("LOAD: "),
      
      #user input: selection and upload of CSV file factors
      fileInput(inputId = "fileIn", label = "Upload CSV Data File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,
                           text/plain",
                           ".csv"),
                width = NULL),
      
      #visual horizontal line 
      tags$hr(),
      
      #user input: display headers from CSV
      checkboxInput("header", "Header", TRUE),
      
      #user input: selecting CSV separator
      radioButtons("sep", "Separator",
                   choices = c(Semicolon = ";",
                               Comma = ",",
                               Tab = "\t"),
                   selected = ","),
      
      helpText("EXPLORE: "),
      
      #user input: selection dropdown for CSV columns
      selectInput("selectIn", "Select Column (Attribute) to Explore: ", choices = "Waiting for Upload... "),
      
      #visual horizontal line 
      tags$hr(),
      
      
      helpText("REFRESH (Caution!): "),
      
      #Javascript refresh button, available for entire process as reset functionality
      actionButton("refresh", "Restart")
      
              
    ),
    
    #sidebar, ouputs
    mainPanel(
      
      #set tabs for page
      tabsetPanel(
        
        ##
        ###PANEL: LOAD###
        ###See server for corresponding comment and backend functionality###
        ##
        
        tabPanel("Load", 
                 #Javascript dynamic output
                 dataTableOutput("table")
        ),
        
        ##
        ###PANEL: EXPLORE###
        ###See server for corresponding comment and backend functionality###
        ##
        
        tabPanel("Explore", 
                 helpText("Data frame summary: "),
                 
                 #displays summary of dataset in text
                 verbatimTextOutput("summary"), 
                 
                 hr(),
                 
                 helpText("Data frame histograms (data density): "),
                 
                 
                 #outputs ggplot histogram
                 plotOutput("histogram")
        ),
        
        ##
        ###PANEL: CLEAN###
        ###See server for corresponding comment and backend functionality###
        ##
        
        tabPanel("Clean",
                 
                 #defines column
                 fluidRow(
                   column(width = 4,
                          helpText("Change datatype (to numeric or to character): "),
                          
                          #user input: datatype converter
                          radioButtons("dataType", "Data type converter", 
                                       c(toNumeric = 'To Numeric',
                                         toCharacter = 'To Character')
                                       ),
                          
                          #displays that execution of datatype conversion is complete
                          verbatimTextOutput("dataConfirm")
                          
                   ),
                   
                   
                   
                   
                   column(width = 4, offset = 1,
                          
                          helpText("Select column, then choose cleaning method: "),
                          helpText("Removes all rows with NULLs in column."), 
                          
                          #output of null columns removal, head only, 
                          verbatimTextOutput("nullCount"),
                          verbatimTextOutput("nullCount_2"),
                          
                          #user input: binary for whether to remove nulls
                          checkboxInput("null", "Remove NULLs", FALSE)
                          
                   )
                 ),
                 
                
                 br(),
                 
                 hr(),
                 
                 
                 helpText("Impute NULL values in selected column."), 
                 
                 #user input: selecting type of null imputation
                 radioButtons("null_imp", "Imputation method", 
                              c(Mean = 'mean',
                                Median = 'median',
                                Mode = 'mode')),
                 
                 verbatimTextOutput("nullImpute"),
                 
                 #user input: execute selected imputation method on selected column
                 actionButton("impute", "Impute"),
                 verbatimTextOutput("nullImpute_2")
                 
                 
                 
        ),
        
        ##
        ###PANEL: PREDICT###
        ###See server for corresponding comment and backend functionality###
        ##
        
        tabPanel("Predict",
                 fluidRow(
                   column(width = 5,
                          helpText("Select features for training model: "),
                          
                          #user input: multiple selection of prediction features 
                          checkboxGroupInput("selectFeatures", "Select Features:",
                                             c(features = "features")
                          )
                                              
                          ),
                    column(width = 5, offset = 1,
                           
                           #user select: target variable selection
                           helpText("Select target (not one of the selected features): "),
                           
                           radioButtons("selectTarget", "Select Target: ",
                                        c(targets = "targets")
                           ),
                           
                           numericInput("train", "Choose training set size (20% to 80%).  Test will comprise remainder: ",
                                        value = 50, min = 20, max = 80),
                           
                           actionButton("predict", "Execute (tree regressor)")
                          )
                   ),
                 
                  hr(),
                 
                  helpText("Model output, note coefficients: "),
                  plotOutput("model")
        ),
        
        
        ##
        ###PANEL: SHARE###
        ###See server for corresponding comment and backend functionality###
        ##
        
        tabPanel("Share",
                 helpText("This is a work-in-progess project.  Your input on this project will be greatly appreciated: "),
                 
                 #user input: rating for app (1-5), higher number equates to better rating
                 numericInput("shareRating", "Rating Value (5-Best, 1-Worst)", value = 3, min = 1, max = 5),
                 
                 helpText("Please share one feature you would like to see added to the next version: "),
                 
                 #user input: selecting feature most desired to be added or improved
                 radioButtons("shareFeature", "Features",
                              c(filter_methods = "filterMethods",
                                longwide_conversion = "longWideConversion",
                                visuals = "visuals",
                                algorithms = "algorithms",
                                refinement = "refinement",
                                error_measure = "errorMeasure")
                 ),
                 
                 #user input: executes rating and feature feedback to be sent via email
                 actionButton("shareSend", "Send (Thanks)")
        ),
        
        ##
        ###PANEL: EXPLORE PLUS###
        ##
        
        tabPanel("(Coming Soon) Explore +",
                 
                 br(),
                 
                 helpText("TBD: Simple dashboarding.  Only with numeric columns and single variate.")
        )
        
                 
        
        
      )
    )
        
    
  )

)
  

  



  
server <- function(input, output, session){

##
###PANEL: LOAD###
##
  
  #reactive, CSV input field, stores are variable
  workingDf <- reactive({
    rawDf <- input$fileIn
    
    req(rawDf)
    
    # if (is.null(rawDf))
    #   return(NULL)
    
    lookDf <- read.csv(rawDf$datapath, header = input$header, sep=input$sep)
    
    return(lookDf)
  })
  

###REFRESH BUTTON action, restarts process at anytime###  
  observeEvent(input$refresh, {
    shinyjs::js$refresh()
  })
  
  
##   
###PANEL: LOAD###
##
  
  #Render dynamic, interative table from JavaScript library: DataTables
  output$table <- renderDataTable({
    workingDf()
  }
  , options = list(pageLength = 10)
  )
  
##
###PANEL: SIDE/MAIN###
##
  
  observe({
    updateSelectInput(session, "selectIn", choices = colnames(workingDf()))
  })
  
  
## 
###PANEL: EXPLORE###
##
  
  output$histogram <- renderPlot({
    tempDf <- workingDf()
     
     #req(input$SelectIn)
     #histDf <- tempDf[, sapply(tempDf,is.numeric)]
     
     
     # hist(
     #   x = tempDf[,input$SelectIn],
     #   main = input$selectIn,
     #   border = 'blue',
     #   xlab = "Temporary X Axis"
     # 
     # )
  
    ggplot(workingDf(),aes(tempDf[,input$selectIn])) + geom_histogram() + xlab(input$selectIn)
  
  })
  
##
###PANEL: SIDE/MAIN###
##
  
  observeEvent(input$refresh, {
    session$reload()
  })
  
  
##
###PANEL: EXPLORE###
##
  
  output$summary <- renderPrint({
    #summData <- workingDf()
    summary(workingDf())
  })
  

##
###PANEL: CLEAN###
##
  
  output$dataType <- renderText({
    
    if(input$dataType == "To Character"){
      workingDf()[,input$SelectIn] <- as.character(workingDf()[,input$SelectIn])
      output$dataConfirm <- renderPrint({
        "Completed, to numeric"
      })
    }
    else if(input$dataType == "To Numeric"){
      workingDf()[,input$SelectIn] <- as.numeric(workingDf()[,input$SelectIn])
      output$dataConfirm <- renderPrint({
        "Completed, to numeric"
      })
    }
    
    
    
  })
  
  
##
###PANEL: CLEAN###
##  
  
  output$nullCount <- renderText({
    
    paste("NULLs in", as.character(input$selectIn), ":", sum(is.na(workingDf()[,input$selectIn])))
    
  })
  
  
##
###PANEL: CLEAN###
## 
  
  output$nullCount_2 <- renderPrint({
    naimpDf <- workingDf()
    
    if(input$null == TRUE){
      
    workingDf()[!is.na(workingDf()[,input$selectIn]),]
    
    head(workingDf())
      
    }
    
  })
  
  
##
###PANEL: CLEAN###
## 
  
  output$nullImpute <- renderPrint({
    imputeDf <- workingDf()
    
    if(null_imp$mean){
      #Impute selected column with mean value (if column is numeric)
      imputeDf[,input$selectIn] <- with(imputeDf, impute(imputeDf[,input$selectIn], mean))
    }
    
    else if(null_imp$median){
      #Impute selected column with median value (if column is numeric)
      imputeDf[,input$selectIn] <- with(imputeDf, impute(imputeDf[,input$selectIn], median))
    }
    
    else if(null_imp$mode){
      #Impute selected column with mode value
      imputeDf[,input$selectIn] <- with(imputeDf, impute(imputeDf[,input$selectIn], mode))
    }
    
  })
  
##
###PANEL: PREDICT###
##
  
  
  observe({
    updateCheckboxGroupInput(session, "selectFeatures", choices = colnames(workingDf()))
  })
  
##
###PANEL: PREDICT###
##
  
  observe({
    updateRadioButtons(session, "selectTarget", choices = colnames(workingDf()))
  })

}



shinyApp(ui = ui, server = server)


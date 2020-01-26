#
##
###DataOneTwo web app
###Ingests CSV data files, cleans and predicts data
###VERSION 1.0.1
###December, 2018
###Updated: January, 2020
###See license.md for MIT License
###Isayev, Alex (main contributor)
##
#

##Install necessary packages
##Project is package version agnostic as of VERSION 1.0.

#shiny, web app
#shinythemes, thematic aesthetics of shiny web app
#dplyr, data manipulation
#ggplot2, data visualization
#shinyjs, shiny-to-Javascript functionality
#Hmisc, data manipulation (imputation)
#V8, Javascript engine
#rpart, predictive modeling
#zeallot, dependency for modelr 0.1.5 since install fails to load it
#modelr, partitioning and modeling data
#DT for data tables that communicate with JavaScripts indexing

require(devtools)

install.packages("shiny")
install.packages("shinythemes")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("shinyjs")
install.packages("Hmisc")
install.packages("rpart")
install.packages("zeallot")
install.packages("modelr")
#install_version("modelr", version = "0.1.2", repos = "http://cran.us.r-project.org")
install.packages("DT")


##manages explicit Javascript functionality
install.packages("V8")

##Load installed libraries

library(shiny)
library(shinythemes)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(V8)
library(rpart)
library(zeallot)
library(modelr)
library(DT)

#set Javascript functionality for page refresh, store as 'jscode'
jscode <- "shinyjs.refresh = function() { location.reload(); }"

##UI design
ui <- fluidPage(
  
  #extends defined 'jscode' to Shiny, allowing for Javascript page refresh functionality
  useShinyjs(),
  extendShinyjs(text = jscode),
  
  #CSS theme
  theme = shinytheme("united"),
  
  # Title for page
  titlePanel("DataOneTwo"),
  
  #sidebar, complete layout
  sidebarLayout(
    
    #sidebar, inputs
    #these elements remain throughout page interactions/experience
    sidebarPanel(
      
      helpText("LOAD (up to 5MB): "),
      
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
        ###PANEL: LOAD
        ###See server for corresponding comment and backend functionality###
        ##
        
        tabPanel("Load", 
                 #Javascript dynamic output
                 dataTableOutput("table")
        ),
        
        ##
        ###PANEL: EXPLORE
        ###See server for corresponding comment and backend functionality###
        ##
        
        tabPanel("Explore", 
                 helpText("Data frame summary: "),
                 
                 #displays summary of dataset in text
                 verbatimTextOutput("summary"), 
                 
                 #visual horizontal line 
                 hr(),
                 
                 helpText("Data frame histograms (data density): "),
                 
                 
                 #outputs ggplot histogram
                 plotOutput("histogram")
        ),
        
        ##
        ###PANEL: CLEAN
        ###See server for corresponding comment and backend functionality###
        ##
        
        tabPanel("Clean",
                 
                 #defines column
                 fluidRow(
                   #column for data type conversion
                   column(width = 4,
                          helpText("Change datatype (to numeric or to character): "),
                          
                          #Warning for character type conversions
                          helpText("WARNING: If you convert a non-numeric character to a numeric, please press the Undo All button"),
                          
                          #user input: datatype converter (numeric or character)
                          radioButtons("dataType", "Data type converter", 
                                       c("To Numeric",
                                         "To Character",
                                         "To Factor"), 
                                       inline = TRUE
                          ),
                          
                          #user input: execute conversion from 'dataType' input
                          actionButton('typeConvert', 'Execute Conversion'),
                          
                          #visual horizontal line 
                          hr()
                          
                          
                   ),
                   
                   
                   
                   #column for null removal
                   column(width = 4, offset = 1,
                          
                          helpText("Select column, then choose cleaning method: "),
                          helpText("Removes all rows with NULLs in column."), 
                          
                          #output of null columns removal, head only, 
                          verbatimTextOutput("nullCount"),
                          
                          
                          #user input: binary for whether to remove nulls
                          actionButton('null', 'Null Remove'),
                          
                          actionButton('nullAll', 'Null Remove (All)'),
                          
                          hr()
                          
                          
                   ),
                   
                   #column for imputation
                   #user input: type of imputation method
                   radioButtons('imputeSelect', "Select Imputation Method: ",
                                c("mean", "median", "mode")
                   ),
                   
                   #user input: executes imputation from 'imputeSelect'
                   actionButton('imputExecute', "Impute Column"),
                   
                   #visual horizontal line 
                   hr(),
                   
                   #user input: reloads uploaded CSV as dataframe, undoes all cleaning
                   actionButton('undo', 'Undo All Cleaning')
                   
                   
                 )
        ),
        ##
        ###PANEL: PREDICT
        ###See server for corresponding comment and backend functionality###
        ##
        
        tabPanel("Predict",
                 fluidRow(
                   column(width = 5,
                          helpText("Model: Tree Regressor"),
                          helpText("DISCLAIMER: Please use 'Null Remove (All)' for optimal results"),
                          helpText("Select features for training model: "),
                          
                          #user input: multiple selection of prediction features 
                          checkboxGroupInput("selectFeatures", "Select Features:",
                                             c(features = "features")
                          )
                          
                   ),
                   column(width = 5, offset = 1,
                          helpText("Select target (not one of the selected features): "),
                          
                          #user input: target variable selection
                          radioButtons("selectTarget", "Select Target: ",
                                       c("targets")
                          ),
                          
                          #user input: train/test split
                          numericInput("train", "Choose training set size (20% to 80%).  Test will comprise remainder: ",
                                       value = 50, min = 20, max = 80),
                          
                          #user input: executes predictive modelling from 'train' and 'selectTarget' inputs
                          actionButton("predict", "Execute (tree regressor)"),
                          verbatimTextOutput("modelError")
                   )
                 ),
                 
                 hr(),
                 
                 helpText("Model output, note coefficients: "),
                 #displays modelled tree regressor, with MAE as title
                 plotOutput("model"),
                 #not in use
                 verbatimTextOutput("maErr")
        ),
        
        tabPanel("Share (Coming Soon)",
                 helpText("Ability to rate and share DataOneTwo.  Release Date: TBD")
        )
      )
    )
  )
)









#server side of Shiny app
#arguments are inputs and outputs of UI and session for use session
server <- function(input, output, session){
  
  ##
  ###INITIALIZATION & UPDATE: SIDE PANEL
  ##
  
  #initialize reactive values as dynamic variable from CSV
  values <- reactiveValues(df = NULL)
  
  #set 'values' variable to take uploaded filed from 'fileIn' as CSV
  observeEvent(input$fileIn, {
    values$df <- read.csv(input$fileIn$datapath, na.strings=" ", header = input$header, sep= input$sep)
  })
  
  #updates 'selectIn' UI when CSV upload is complete
  observe({
    updateSelectInput(session, "selectIn", choices = colnames(values$df))
  })
  
  ###'refresh' button action, restarts session at any time
  observeEvent(input$refresh, {
    shinyjs::js$refresh()
  })
  
  
  
  ##
  ###PANEL: LOAD
  ###See corresponding UI commented section
  ##
  
  #render dynamic, interative table from JavaScript library: DataTables
  output$table <- renderDataTable({
    values$df
  }
  , options = list(pageLength = 10)
  )
  
  
  
  ##
  ###PANEL: EXPLORE
  ##
  
  #renders summary text of 'value$df' and its attributes/columns
  output$summary <- renderPrint({
    summary(values$df)
  })
  
  #generates ggplot powered histogram from 'values$df' based on 'selectIn' column
  output$histogram <- renderPlot({
    ggplot(values$df,aes(values$df[,input$selectIn])) + geom_histogram() + xlab(input$selectIn)
  })
  
  
  ##
  ###PANEL: CLEAN
  ###See corresponding UI commented section
  ##
  
  #renders text of null value count from 'selectIn' column
  output$nullCount <- renderText({
    values$df[,input$selectIn][values$df[,input$selectIn] == ""] <- NA
    sum(is.na(values$df[,input$selectIn]))
  })
  
  #when 'null' action button is clicked, rows containing null values in 'selectIn' column are removed
  #first converts any empty strings to NA values (REWORK)
  observeEvent(input$null, {
    values$df[,input$selectIn][values$df[,input$selectIn] == ""] <- NA
    values$df <- values$df[!(is.na(values$df[,input$selectIn])),]
    
  })
  
  observeEvent(input$nullAll, {
    #values$df[!(is.na(values$df)),]
    values$df = na.omit(values$df)
  })
  
  
  #defines condition to be stored in 'dataChange' when 'dataType' value is selected on 'selectIn' column choice
  #Converts data type of 'selectIn' column to either numeric or character
  dataChange <- eventReactive(input$dataType, {
    if(input$dataType == "To Numeric"){
      values$df[,input$selectIn] = as.numeric(values$df[,input$selectIn])
    }
    else if (input$dataType == "To Character") {
      values$df[,input$selectIn] = as.character(values$df[,input$selectIn])
    }
    else if (input$dataType == "To Factor") {
      values$df[,input$selectIn] = as.factor(values$df[,input$selectIn])
    }
  })
  
  #when 'typeConvert' is clicked, it executes event defined in 'dataType' variable/function
  observeEvent(input$typeConvert, {
    dataChange()
    
  })
  
  
  #defines condition to be stored in 'imputation' when 'imputeSelect' option is selected on 'selectIn' column choice
  #imputes dataframe 'values$df' by mean, median or mode method
  imputation <- eventReactive(input$imputeSelect, {
    if(input$imputeSelect == "mean"){
      values$df[,input$selectIn] = with(values$df, impute(values$df[,input$selectIn], mean))
    }
    else if(input$imputeSelect == "median"){
      values$df[,input$selectIn] = with(values$df, impute(values$df[,input$selectIn], median))
    }
    else if(input$imputeSelect == "mode"){
      values$df[,input$selectIn] = with(values$df, impute(values$df[,input$selectIn], mode))
    }
  })
  
  #when 'imputExecute' is clicked, it executes event defined in 'imputation' variable/function
  observeEvent(input$imputExecute, {
    imputation()
  })
  
  #resets 'values' variable when 'undo' button is clicked, reloading CSV file
  observeEvent(input$undo, {
    values$df <- read.csv(input$fileIn$datapath, header = input$header, sep= input$sep)
  })
  
  
  ##
  ###PANEL: PREDICT
  ##See corresponding UI commented section
  
  #updates values of 'selectFeatures' when CSV 'values$df' is uploaded
  observe({
    updateCheckboxGroupInput(session, "selectFeatures", choices = colnames(values$df))
  })
  
  #updates values of 'selectTarget' when CSV 'values$df' is uploaded
  observe({
    updateRadioButtons(session, "selectTarget", choices = colnames(values$df))
  })
  
  #defines modeling to be performed
  #1. convert training user input into appropriate decimal
  #2. compute test set from training (1 - train)
  #3. define errorFun(), which serves to inform user if model has error occur
  #4. partitions train/test sets
  #5. fits model based on given user input features on user input target variable
  #inputs 'selectTarget', 'selectFeatures' and 'train' selections by user
  model <- eventReactive(c(input$selectTarget, input$selectFeatures, input$train), {
    
    #convert user input into decimal for train set  
    trainSize = as.numeric(input$train) / 100
    #1 - train = test set
    testSize = 1 - trainSize
    
    
    #setting seed for replication purposes
    set.seed(011)
    
    #creating formula for rpart() to take in 'input$selectFeatures' and 'input$selectTarget' in appropriate format
    features <- paste(as.vector(input$selectFeatures), collapse="+")
    formula <- as.formula(paste(input$selectTarget," ~ ", features, sep = ""))
    
    #partitions 'values$df' into train/test set based on train set size in 'input$train'
    splitData <- modelr::resample_partition(values$df, c(test = testSize, train = trainSize))
    
    #fit tree regressor with training dataset
    fitted <- rpart(formula, data = splitData$train, control =rpart.control(minsplit =1,minbucket=1, cp=0))
    
    #measures mean absolute error of model
    final <- modelr::mae(fitted, splitData$test)
    
    #plots tree regressor, with 'final' MAE as title
    plot(fitted, main = paste("Mean Absolute Error: ", final))
    text(fitted, cex=.6)
    
  })
  
  
  #execute 'model' when 'predict' button is clicked and render as plot
  observeEvent(input$predict,{
    output$model <- renderPlot({
      model()
      # output$maErr <- renderText({
      #   model()
    })
  })
  
  
  
}


#executes shiny application
shinyApp(ui = ui, server = server)





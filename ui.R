library(shiny)
library(DT)

# constants
github.url <- 'https://github.com/IndraP24/interactive_data_analyser'

# Disable shiny widget, from:
# https://groups.google.com/forum/#!topic/shiny-discuss/uSetp4TtW-s
disable <- function(x) {
  if (inherits(x, 'shiny.tag')) {
    if (x$name %in% c('input', 'select'))
      x$attribs$disabled <- 'disabled'
    x$children <- disable(x$children)
  }
  else if (is.list(x) && length(x) > 0) {
    for (i in 1:length(x))
      x[[i]] <- disable(x[[i]])
  }
  x
}
# function to generate the complete model summary in a single string
completeModelSummary <- function() {
  'TODO - This will be the model summary'
}


shinyUI(
  fluidPage(titlePanel('Auto-Vis-ML - Automated Data Visualiser and ML Model Trainer'),

    navbarPage('',

      # Problem Description
      tabPanel('0. About',
               fluidPage(
                 titlePanel(p("About the Project", style = "color:#3474A7")),
                 h4('Summary'),
                 p('This Shiny App is an Automated Platform that helps user to automate data visualization 
                      and model training evaluation on different machine learning models as applied to the selected Data of Choice'),
                 br(), br(),
                 h4('Steps to complete and evaluate a machine learning model'),
                 tags$ol(
                   tags$li('Select the data - choose the data you want to use'),
                   tags$li('Examine the data summary - see what is in the data'),
                   tags$li('Explore the data - see what features to use in a model'),
                   tags$li('Build a prediction model - pre-process data, select features, and generate model'),
                   tags$li('Evaluate prediction model - estimate in-sample and out-of-sample errors'),
                   tags$li('Predict outcomes for test data')
                 ),
                 br(), br(),
                 h4('Source Code and some sample data available'),
                 a(github.url),
                 br(), br(),
                 br(), br()
               )
      ),
      
      # upload the data and display a preview
      tabPanel('1. Upload',
        fluidPage(
          titlePanel(p("Upload the Data", style = "color:#3474A7")),
          sidebarLayout(
            sidebarPanel(
              fileInput(inputId = "upload",
                        label = "Upload data (.csv file only!)",
                        accept = c(".csv")),
              br(),
              uiOutput("target")
            ),
            mainPanel(
                  h4('Data Uploaded:'),
                  tableOutput("files"),
                br(), br(),
                  h4('Data Preview:'),
                  DTOutput("head")
            )
          )
        )
      ),

      # show data summary of all data - give options to see raw or processed data
      # TODO need doc as to what you are looking at
      # separate outcomes from predictors
      tabPanel('2. Data Summary',
        titlePanel(p("Data Summary", style = "color:#3474A7")),
        sidebarLayout(
          sidebarPanel(
            uiOutput(outputId = "dropSelected"),
            actionButton("drop", "Drop Columns"),
            br(), br(),
            p("Press the button below to conduct basic preprocessing:"),
            p("1. Remove rows with missing data, 2. Label Encode character data"),
            actionButton("preprocess", "Preprocess Data")
          ),
          mainPanel(
            h4('Features'),
            tableOutput('PredictorsSummaryOut'),
            h4('Target'),
            tableOutput('OutcomeSummaryOut')
          )
        )
      ),
      
      # explore the data
      tabPanel('3. Explore Data',
        sidebarLayout(
          sidebarPanel(
            selectInput('singlePlotGeom', 'Select plot type', 
              choices=c('point', 'boxplot', 'histogram', 'density', 'jitter'),
              selected='jitter'),
            uiOutput('expXaxisVarSelector'),
            uiOutput('expYaxisVarSelector'),
            uiOutput('expColorVarSelector')
          ),
          mainPanel(
            h4('One and Two Variable Plot'),
            plotOutput('expSinglePlot'),
            h4('Pairs Plot (only non-zero variance variables shown)'),
            plotOutput('expPairsPlot', width='100%', height='800px')
          )
        )
      ),
     # build model
      tabPanel('4. Build Prediction Model',
        sidebarLayout(
          sidebarPanel(
            selectInput('preProcessMethods', 'Select data preprocessing method(s)',
              choices=c(
                'Center Data' = 'center', 
                'Scale Data' = 'scale', 
                'Box Cox Transform Data' = 'BoxCox',
                'Yeo-Johnson Transform Data' = 'YeoJohnson',
                'Inpute missing data with k-nearest neighbors' = 'knnImpute',
                'Principle Component Analysis (95% variance)' = 'pca'
              ),
              selected='BoxCox', 
              multiple=TRUE
            ),
            uiOutput('featureSelectInput'),
            selectInput('fracTrain', 'Select the fraction of Training Data',
              choices=c(
                "90%" = 0.90,
                "85%" = 0.85,
                "80%" = 0.80,
                "75%" = 0.75,
                "70%" = 0.70
              ),
              selected=0.90
            ),
            radioButtons('mltype', "Choose the type of Machine Learning:",
                         choices = c("Regression"="reg", "Classification"="clf"), 
                         selected = "reg"),
            uiOutput('machAlgorithm')
          ),
          mainPanel(
            h4('Final model fit'),
            verbatimTextOutput('finalModel'),
            h4('Summary'),
            verbatimTextOutput('summaryModel')
          )
        )
      ),

      # Evaluate model
      tabPanel('5. Model Evaluation',
        fluidRow(
          column(6,
            wellPanel(
              h4('Estimated In Sample Accuracy (within training data)'),
              verbatimTextOutput('inSampleAccuracy')
            )
          ),
          column(6,
            wellPanel(
              h4('Estimated Out of Sample Accuracy (within verification data)'),
              verbatimTextOutput('outOfSampleAccuracy')
            )
          )
        )
      ),
  
      tabPanel('6. Test Data Results',
        h4('Predicted survival of individuals in the test data set'),
          tableOutput('testPredictions')
      )

    )
  )
)


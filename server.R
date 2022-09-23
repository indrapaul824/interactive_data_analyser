# some libs for shinyapps to deploy correctly
library(shiny)
library(curl)
library(e1071)
library(data.table)
library(caret)
# models for caret, need here explicitly for shinyapps deployment
library(randomForest) # for rf
library(gbm)    # for gbm
library(mboost) # for glmboost
library(klaR)   # for nb
library(plyr)
library(dplyr)
library(psych)
library(purrr)


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

shinyServer(
  function(input, output) {
    ## Data Upload
    dataInput <- reactive({
      req(input$upload)
      
      ext <- tools::file_ext(input$upload$name)
      switch(ext,
             csv = vroom::vroom(input$upload$datapath, delim = ","),
             validate("Invalid file; Please upload a .csv file")
      )
    })
    output$target <- renderUI({
      selectInput('target', 'Select the Outcome/Target Variable', 
                  choices=as.list(colnames(dataInput())))
    })
    output$files <- renderTable(input$upload)
    output$head <- DT::renderDT(
      dataInput(), extensions = 'Buttons', filter = "top", editable=T, rownames=F,
      options = list(
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        lengthMenu = list(c(10, 50, 100, -1), c(10, 50, 100, "All"))
      )
    )
    
    encode_ordinal <- function(x, order = unique(x)) {
      x <- as.numeric(factor(x, levels = order, exclude = NULL))
      x
    }
    
    data <- reactiveVal()
    
    observeEvent(input$upload, ignoreNULL = T, ignoreInit = T, {
      file1 <- input$upload
      if (is.null(file1)){return()}
      data(read.csv(file=file1$datapath))
    })
    
    observeEvent(input$drop, {
      claim <- data()
      drop <- c(input$dropCols)
      claim <- claim[, !names(claim) %in% drop]
      data(claim)
    })
    
    observeEvent(input$preprocess, {
      claim <- data()
      claim <- claim[complete.cases(claim),]
      colTypes <- map(claim, class)
      
      for (col in colnames(claim)) {
        if (colTypes[col] == 'character') {
          claim[[col]] <- encode_ordinal(claim[[col]])
        }
      }
      data(claim)
    })
    
    
    
    
    ## Data Summary
    output$dropSelected <- renderUI({
      selectInput("dropCols", "Select the Columns you want to drop:",
                  choices=as.list(colnames(data())), multiple = T)
    })
    
    output$PredictorsSummaryOut <- renderTable({
      describe(data()[, !(names(data()) %in% c(input$target))])
    }, rownames = T)
    output$OutcomeSummaryOut <- renderTable({ 
      describe(data()[input$target])
    }, rownames = T)

    
    
    ## Explore Data
    # pairs plot - always
    output$expPairsPlot <- renderPlot({
      featurePlot(x=data(), 
        y=data()[input$target], 
        plot='pairs', auto.key=list(columns=2))
    })
    # generate variable selectors for individual plots
    # ideas from https://gist.github.com/jcheng5/3239667
    output$expXaxisVarSelector <- renderUI({
      selectInput('expXaxisVar', 'Variable on x-axis', 
        choices=as.list(colnames(data())), selected=colnames(data())[1])
    })
    # generate variable selectors for individual plots
    getYaxisVarSelector <- function(geom) { 
      # wy = wtih y, wo = without y (or disable)
      widget <- selectInput('expYaxisVar', 'Variable on y-axis', 
        choices=as.list(colnames(data())), selected=colnames(data())[2])
      wy <- widget
      woy <- disable(widget)
      switch(geom,
        point = wy,
        boxplot = wy,
        histogram = woy,
        density = woy,
        jitter = wy
      )
    }
    output$expYaxisVarSelector <- renderUI({
      getYaxisVarSelector(input$singlePlotGeom)
    })
    output$expColorVarSelector <- renderUI({
      selectInput('expColorVar', 'Variable to color by', 
        choices=as.list(c('None', colnames(data()))),
        selected=input$target)
    })
    # create ggplot statement based on geom
    add_ggplot <- function(geom) {
      gx <- ggplot(data(), aes_string(x=input$expXaxisVar))
      gxy <- ggplot(data(), aes_string(x=input$expXaxisVar, y=input$expYaxisVar))
      switch(geom,
        point = gxy,
        boxplot = gxy,
        histogram = gx,
        density = gx,
        jitter = gxy
      )
    }
    # create ggplot geom
    add_geom <- function(geom) {
      switch(geom,
        point = geom_point(aes_string(color=input$expColorVar)),
        boxplot = geom_boxplot(aes_string(color=input$expColorVar)),
        histogram = geom_histogram(aes_string(color=input$expColorVar)),
        density = geom_density(aes_string(color=input$expColorVar)),
        jitter = geom_jitter(aes_string(color=input$expColorVar))
      )
    }
    output$expSinglePlot <- renderPlot({
      g <- add_ggplot(input$singlePlotGeom) + add_geom(input$singlePlotGeom)
      print(g)
    })

    ## Prediction Model
    f <- reactive({
      as.formula(paste(input$target, "~."))
    })
    # create feature selection
    output$featureSelectInput <- renderUI({
      selectInput('featureSelect', 'Select features to generate model', 
        choices=as.list(colnames(data()[, !(names(data()) %in% c(input$target))])),
        multiple = TRUE, selected=c(colnames(data())[1], colnames(data())[2], colnames(data())[3]))
    })
    output$machAlgorithm <- renderUI({
        selectInput('machLearnAlgorithm', 
                    'Select the model or machine learning algorithm',
                    choices= c('K-Nearest Neighbors' = 'knn',
                               'Generalized Linear Model (logit)' = 'glm',
                               'Random Forests (may take a few minutes)' = 'ranger',
                               'Gradient Boosting' = 'gbm',
                               'Boosted Generalized Linear Model' = 'glmboost',
                               'Linear Discriminant Analysis' = 'lda',
                               'Naive Bayes' = 'nb'), 
                    selected='knn')
    })
    
    #split the data into train and test
    splitSlider <- reactive({
      input$fracTrain / 100
    })
    trainRowIndex <- reactive({
      sample(1:nrow(data()), splitSlider() * nrow(data()))
    })
    
    trainData <- reactive({
      train_dt <- data()
      train_dt <- train_dt[trainRowIndex(),]
    })

    testData <- reactive({
      test_dt <- data()
      test_dt <- test_dt[-trainRowIndex(),]
    })

    output$cntTrain <- renderText({
      paste0("Training set: ", nrow(trainData()), " records")
    })
    output$cntTest <- renderText({
      paste0("Test set: ", nrow(testData()), " records")
    })

    # apply model to training set
    applyModel <- function(modelType, features) {
      df <- trainData()
      if (input$mltype == "clf") {
        # df$input$target <- as.factor(df$input$target)
        # Convert target variable from numeric to factor
        df[[input$target]] <- as.factor(df[[input$target]])
        if (modelType == 'gbm')
          train(f(), 
            data=select(df, one_of(c(input$target, features))), 
            method=modelType, preProcess=input$preProcessMethods, verbose=F, metric='Accuracy')
        else
          train(f(), 
            data=select(df, one_of(c(input$target, features))), 
            method=modelType, preProcess=input$preProcessMethods, metric='Accuracy')
      }

      else {
        if (modelType == 'gbm')
          train(f(), 
            data=select(df, one_of(c(input$target, features))), 
            method=modelType, preProcess=input$preProcessMethods, verbose=F, metric='RMSE')
        else
          train(f(), 
            data=select(df, one_of(c(input$target, features))), 
            method=modelType, preProcess=input$preProcessMethods, metric='RMSE')
      }
    }
    # reactive functions to run and evaluate model
    runModel <- reactive({
      applyModel(input$machLearnAlgorithm, input$featureSelect)
    })
    # if summary(fit) has names, use it, if not, do not
    output$summaryModel <- renderPrint({
      if (!is.null(names(summary(runModel()))))
        summary(runModel())
      else
        'Same as Final Model Fit above'
    })
    # summary of final model
    output$finalModel <- renderPrint({
      runModel()
    })

    ## Prediction Model Evaluation
    evalModel <- function(testData, features) {
      predictions <- predict(runModel(), select(testData, one_of(features)))
      truthes <- testData[, input$target]
      # generate confusion matrix
      if (input$mltype == "clf") {
        confusionMatrix(predictions, as.factor(truthes))
      }
      else {
        # Convert target variable from factor to numeric
        truthes <- as.numeric(truthes)
        # calculate RMSE
        rmse <- sqrt(mean((truthes - predictions)^2))
        # calculate R^2
        r2 <- cor(truthes, predictions)^2
        # calculate MAE
        mae <- mean(abs(truthes - predictions))
        # calculate MASE
        mase <- mean(abs(truthes - predictions))/mean(abs(diff(truthes)))
        # calculate adjusted R^2
        adjr2 <- 1 - (1 - r2)*(nrow(testData) - 1)/(nrow(testData) - length(features) - 1)
        # return results
        c(rmse, r2, mae, mase, adjr2)
      }
    }
    #training data accuracy
    output$inSampleAccuracy <- renderPrint({
      df <- trainData()
      df <- if (input$mltype == "clf") {
        # Convert target variable from numeric to factor
        df[, input$target] <- as.factor(df[, input$target])
        df
      }
      else
        df
      evalModel(df, input$featureSelect)
    })

    output$inSampleAccuracyPlot <- renderPlot({
      df <- trainData()
      df <- if (input$mltype == "clf") {
        # Convert target variable from numeric to factor
        df[, input$target] <- as.factor(df[, input$target])
        df
      }
      else
        df
      if (input$mltype == "clf") {
        # plot confusion matrix
        plot(confusionMatrix(predict(runModel(), select(df, one_of(input$featureSelect))), 
          as.factor(df[, input$target])))
      }
      else {
        # plot ROC curve
        plot(roc(as.numeric(df[, input$target]), predict(runModel(), select(df, one_of(input$featureSelect)))))
      }
    })
    #testing data accuracy
    output$outOfSampleAccuracy <- renderPrint({
      df <- testData()
      df <- if (input$mltype == "clf") {
        # Convert target variable from numeric to factor
        df[, input$target] <- as.factor(df[, input$target])
        df
      }
      else
        df
      evalModel(df, input$featureSelect)
    })

    output$outOfSampleAccuracyPlot <- renderPlot({
      df <- testData()
      df <- if (input$mltype == "clf") {
        # Convert target variable from numeric to factor
        df[, input$target] <- as.factor(df[, input$target])
        df
      }
      else
        df
      if (input$mltype == "clf") {
        # plot confusion matrix
        plot(confusionMatrix(predict(runModel(), select(df, one_of(input$featureSelect))), 
          as.factor(df[, input$target])))
      }
      else {
        # plot ROC curve
        renderPlot(plot(roc(as.numeric(df[, input$target]), predict(runModel(), select(df, one_of(input$featureSelect))))))
      }
    })
  }
)


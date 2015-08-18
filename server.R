library(shiny)
library(ggvis)
library(nlme)
library(shinyAce)
source("helper_functions.R")

shinyServer(function(input, output, session) {
  # server logic to create all the widgets and functionality of the app
  
  the.data <<- NULL
  my.env <<- new.env()
  
  output$read_file <- renderUI({
    # unnamed function to create 'Read the data file' widget
    #
    # Args:
    #   Takes no arguments
    #
    # Returns:
    #   No explicit return. R automatically updates the list like object 'output'
    
    # function to create file upload control wizard
    fileInput("the.file", label = "Read the data file", accept = c ('.csv', '.txt', '.sim', '.dat'))    
  })
  
  output$choose_Xvar <- renderUI({
    # unnamed function to create 'Choose X variable' widget and read the uploaded file
    #
    # Args:
    #   Takes no arguments
    #
    # Returns:
    #   No explicit return. R automatically updates the list like object 'output'
    
    if (is.null(input$the.file))
      return()
    
    # Call the ReadData method to read the field
    the.data <<- ReadData(input$the.file$datapath)
    column.names <- colnames(the.data)

    # built in function to create a box with choices to select from
    selectInput("Xvar", "Choose X variable", choices = c(" ", column.names))
  })
  
  output$choose_Yvar <- renderUI({
    # unnamed function to create 'Choose Y variable' widget
    #
    # Args:
    #   Takes no arguments
    #
    # Returns:
    #   No explicit return. R automatically updates the list like object 'output'
    
    if (is.null(input$the.file))
      return()
    if (is.null(the.data)) { 
      choice.temp <- c(" ", " ")
    } else { 
      choice.temp <- c(" ", colnames(the.data))
    }
    # built in function to create a box with choices to select from
    selectInput("Yvar", "Choose Y variable", choices = choice.temp)
  })
  
  output$choose_IDvar <- renderUI({
    # unnamed function to create 'Choose ID variable' widget
    #
    # Args:
    #   Takes no arguments
    #
    # Returns:
    #   No explicit return. R automatically updates the list like object 'output'
    
    if (is.null(input$the.file))
      return()
    if (is.null(the.data)) { 
      choice.temp <- c(" ", " ")
    } else { 
      choice.temp <- c(" ", colnames(the.data))
    } 
    # built in function to create a box with choices to select from
    selectInput("IDvar", "Choose ID variable", choices = choice.temp )
  })
  
  output$choose_COVvar <- renderUI({
    # unnamed function to create 'Choose Covariate for stratification' widget
    #
    # Args:
    #   Takes no arguments
    #
    # Returns:
    #   No explicit return. R automatically updates the list like object 'output'
    
    if (is.null(input$the.file))
      return()
    if (is.null(the.data)) { 
      choice.temp <- c(" ", " ")
    } else { 
      choice.temp <- c(" ", colnames(the.data))
    }    
    # built in function to create a box with choices to select from
    selectInput("COVvar", "Choose Covariate for stratification", choices  = choice.temp)
  })
  
  output$choose_COVn <- renderUI({
    # unnamed function to create 'Number of COV stratification' widget
    #
    # Args:
    #   Takes no arguments
    #
    # Returns:
    #   No explicit return. R automatically updates the list like object 'output'
    
    if(is.null(input$the.file) | is.null(the.data)) { 
      return()
    } else if(is.null(input$COVvar)) { 
      return()
    } else if(input$COVvar==" ") {  
      return()
    }
    # built in function to create a field to enter numbers
    numericInput("cov.num", "Number of COV stratification", 1)
  })
  
  output$summary <- renderPrint({
    if(is.null(input$the.file) | is.null(the.data)) { 
      return()
    } else { 
      summary(the.data)
    }
  })
  
  output$plot_type <- renderUI({
    fluidRow(
      column(4, offset = 1,
        radioButtons("PlotMethod", h5("Plot type"), c("XY Scatter plot",
                                              "profile plot"))
      )
    )
  })
  
  
  output$plot <- renderUI({
    
    if (is.null(input$the.file) | is.null(the.data) | is.null(input$Xvar)
        | is.null(input$Yvar) | is.null(input$IDvar) | is.null(input$COVvar)) {
      return()
    } else if (input$Xvar==" " | input$Yvar==" " | input$IDvar==" ") {
      return()
    } else  {
      X.name <- input$Xvar
      Y.name <- input$Yvar
      x.lim <- range(the.data[, input$Xvar], na.rm = TRUE)
      y.lim <- range(the.data[, input$Yvar], na.rm = TRUE)
      
      if (input$COVvar == " ") {   
      #  print(input$PlotMethod)            
        if (input$PlotMethod == "XY Scatter plot") {  
          print("Hello")
          DrawScatterPlot(the.data, input$Xvar, input$Yvar, input$IDvar, x.lim, y.lim)
          ggvisOutput("ggvisplot")
        } else if (input$PlotMethod == "profile plot") {  
          DrawProfilePlot(the.data, input$Xvar, input$Yvar, input$IDvar, x.lim, y.lim)
          ggvisOutput("ggvisProfilePlot")
        }  
      } else if (input$cov.num == 0) {
        if (input$PlotMethod == "XY Scatter plot") {  
          DrawScatterPlot(the.data, input$Xvar, input$Yvar, input$IDvar, x.lim, y.lim)
        } else if (input$PlotMethod == "profile plot") {  
          
        }               
      } else { 
        if (input$PlotMethod == "XY Scatter plot") {
          print("Enter Covar")
          DrawScatterPlotWithCovar(the.data, input$Xvar, input$Yvar, input$IDvar, x.lim, y.lim, input$COVvar)
          ggvisOutput("ggvisplot1")
      #    ggvisOutput("ggvisplot_cov")
        }
        
      }
    }
  })
  
  output$plot1<-renderUI({ 
    ggvisOutput("ggvisplot_cov")
  })
  
  
  values <- reactiveValues(binding.list = "No models yet")
  
  update.model.list <- eventReactive(input$eval, {
    temp <- toString(ls.str(my.env))
#    print(class(temp))
    values$binding.list <- strsplit(temp, ", ")
#    print(values$model.list)
#    print(class(values$model.list))
    
    count <- 0
    for(i in 1:length(values$binding.list[[1]])){
      count <- count + 1
      model.name <- values$binding.list[[1]][i]
  #    eval(parse(text = model.name), my.env)
      if(class(eval(parse(text = model.name), my.env)) == "lme"){
        if(model.name %in% values$model.list) {
          index <- match(model.name, values$model.list)
          values$model.list[[1]][index] <- model.name
        } else {
          values$model.list <- c(values$model.list, model.name)
        }
      }
      
    }
  #  values$model.list
  })
  
  write.code.output <- eventReactive(input$eval, {
    eval(parse(text = input$code), my.env)
  })
  
  output$output <- renderPrint({
#    input$eval
    
#    return(isolate(eval(parse(text = input$code), my.env)))
    write.code.output()
  })
  
  
  
#   values.ui <- reactive({
#     invalidateLater(1000, session)
#   
#     print(ls.str(my.env))
#     temp <- toString(ls.str(my.env))
#     text <- strsplit(temp, ", ")
#     print(text)
#     class(text)
#     length(text)
#     values$model.list <- list(values$model.list, text)   # list of models 
#     print("Wrong")
# #    radioButtons("radio", label = "whatever", choices = list("Hello", "hi"))
#     for(i in 1:length(text[[1]])){
#       temp_ag <- text[[1]][i]
#       eval(parse(text = temp_ag), my.env)
#       
#       if(class(eval(parse(text = temp_ag), my.env)) == "lme"){
#         values$model.list <- c(values$model.list, temp_ag)   # updating the model list
#       }
#     }
# 
#     
# #    radioButtons("radio", label = "whatever", choices = values$model.list)
#   })

  create.model.buttons <- eventReactive(input$eval, {
    
    print("hello")
    print("hi")
    print(length(values$model.list))
    if(length(values$model.list) > 0){
      fluidRow(
        column(4, offset = 1,
               radioButtons("PlotMethod", h5("Models Added"), values$model.list)
        )
      )
    }
#    print("bye")
#    print("sure")
  })
  
  output$diagnose <- renderUI({
    input$eval
    update.model.list()
#    values$text
#     radioButtons("radio", label = h3("Radio buttons"),
#              choices = values$model.list)
    create.model.buttons()
    
  })
  

})
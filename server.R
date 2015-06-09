library(shiny)
library(ggvis)
source("helper_functions.R")

shinyServer(function(input, output, session) {
  # server logic to create all the widgets and functionality of the app
  
  the.data <<- NULL
  
  output$read_file <- renderUI({
    # unnamed function to create 'Read the.data file' widget
    # Args:
    #   Takes no arguments
    #
    # Returns:
    #   No explicit return. R automatically updates the list like object 'output'
    
    # function to create file upload control wizard
    fileInput("the.file", label = "Read the data file", accept = c ('.csv', '.txt', '.sim', '.dat'))    
  })
  
  output$choose_Xvar <- renderUI({
    # unnamed function to create 'Choose X variable' widget
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
  
  
  
  output$plot<-renderPlot({    
    if (is.null(input$the.file) | is.null(the.data) | is.null(input$Xvar)
        | is.null(input$Yvar) | is.null(input$IDvar) | is.null(input$COVvar)) {
      return()
    } else if (input$Xvar==" " | input$Yvar==" " | input$IDvar==" ") {
      return()
    } else  {
      X.name <- input$Xvar
      Y.name <- input$Yvar
      x.lim <- range(the.data[, input$Xvar], na.rm=TRUE)
      y.lim<-range(the.data[, input$Yvar], na.rm=TRUE)
      if (input$COVvar == " " | is.null(input$cov.num)) {   
      #  print(input$PlotMethod)            
        if (input$PlotMethod == "XY Scatter plot") {  
          print("Hello")
          DrawScatterPlot(the.data, input$Xvar, input$Yvar, input$IDvar, x.lim, y.lim)
        } else if (input$PlotMethod == "profile plot") {  
          
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
        }
        
      }
    }
  })
})
library(shiny)
library(ggvis)
source("helper_functions.R")

shinyServer(function(input, output) {
  orig.data<<-NULL
  
  output$read_Origfile <- renderUI({
    fileInput("origfile", label = "Read data file", accept =c ('.csv', '.txt', '.sim', '.dat'))    
  })
  
  output$choose_Xvar <- renderUI({
    if (is.null(input$origfile))
      return()
    
    orig.data.name <- paste(input$origfile$datapath, input$origfile$name, sep="/") 
    orig.data <<- ReadPkPdData(input$origfile$datapath)   
    colnames <- colnames(orig.data)
    selectInput("Xvar", "Choose X variable", choices = c(" ", colnames))
  })
  
  output$choose_Yvar <- renderUI({
    if (is.null(input$origfile))
      return()
    if (is.null(input$origfile) | is.null(orig.data)) { 
      choice.temp <- c(" ", " ")
    } else { 
      choice.temp <- c(" ", colnames(orig.data))
    }
    selectInput("Yvar", "Choose Y variable", choices = choice.temp )
  })
  
  output$choose_IDvar <- renderUI({
    if (is.null(input$origfile))
      return()
    if (is.null(input$origfile) | is.null(orig.data)) { 
      choice.temp <- c(" ", " ")
    } else { 
      choice.temp <- c(" ", colnames(orig.data))
    } 
    selectInput("IDvar", "Choose ID variable", choices = choice.temp )
  })
  
  output$choose_COVvar <- renderUI({
    if (is.null(input$origfile)) {
      return()
    }
    if (is.null(input$origfile) | is.null(orig.data)) { 
      choice.temp <- c(" ", " ")
    } else { 
      choice.temp <- c(" ", colnames(orig.data))
    }    
    selectInput("COVvar", "Choose Covariate for stratification", choices  = choice.temp )
  })
  
  output$choose_COVn <- renderUI({
    if (is.null(input$origfile) | is.null(orig.data)) { 
      return()
    } else if (is.null(input$COVvar)) { 
      return()
    } else if (input$COVvar==" ") {  
      return()
    } 
    numericInput("COVn", "Number of COV stratification", 1)
  })
  
  
  
  output$plot<-renderPlot({    
    if (is.null(input$origfile)| is.null(orig.data) | is.null(input$Xvar)
        | is.null(input$Yvar)| is.null(input$IDvar) | is.null(input$COVvar)) {
      return()
    } else if (input$Xvar==" " | input$Yvar==" " | input$IDvar==" ") {
      return()
    } else  {
      X.name <- input$Xvar
      Y.name <- input$Yvar
      x.lim <- range(orig.data[, input$Xvar], na.rm=TRUE)
      y.lim<-range(orig.data[, input$Yvar], na.rm=TRUE)
      if (input$COVvar == " " | is.null(input$COVn)) {   
        print(input$PlotMethod)            
        if (input$PlotMethod == "XY Scatter plot") {  
          DrawScatterPlot(orig.data, input$Xvar, input$Yvar, input$IDvar, x.lim, y.lim)
        } else if (input$PlotMethod == "profile plot") {  
          
        }  
      } else if (input$COVn == 0) {
        if (input$PlotMethod == "XY Scatter plot") {  
          DrawScatterPlot(orig.data, input$Xvar, input$Yvar, input$IDvar, x.lim, y.lim)
        } else if (input$PlotMethod == "profile plot") {  
          
        }               
      } else { 
        
      }
    }
  })
})
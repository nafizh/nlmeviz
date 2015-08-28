library(shiny)
library(ggvis)
library(nlme)
library(shinyAce)
source("helper_functions.R")



shinyServer(function(input, output, session) {
  # server logic to create all the widgets and functionality of the app
  
  my.env <<- new.env()

obs <-   observe({
    if (is.null(input$the.file)){
      print("why here")
      the.data <- ReadData("/Users/Nafiz/Dropbox/Google_Summer_of_Code_2015/nlmeviz/Orthodont.csv")
    } else {
      print("Flaggggg")
    # Call the ReadData method to read the field
      the.data <<- ReadData(input$the.file$datapath)
      s_options <- list()
      s_options <- colnames(the.data)
      
    updateSelectInput(session, "Xvar",
                      choices = s_options,
                      selected = s_options[[2]]
    )
    updateSelectInput(session, "Yvar",
                      choices = s_options,
                      selected = s_options[[3]]
    )
    updateSelectInput(session, "IDvar",
                      choices = s_options,
                      selected = s_options[[4]]
    )
      
    }
  })
    
  output$read_file <- renderUI({
    # unnamed function to create 'Read the data file' widget
    #
    # Args:
    #   Takes no arguments
    #
    # Returns:
    #   No explicit return. R automatically updates the list like object 'output'
    
    # function to create file upload control wizard
#    update.file()
    fileInput("the.file", label = "Read the data file", accept = c ('.csv', '.txt', '.sim', '.dat'))  
    
    
  })
  
  output$choose_Xvar <- renderPrint({
    # unnamed function to create 'Choose X variable' widget and read the uploaded file
    #
    # Args:
    #   Takes no arguments
    #
    # Returns:
    #   No explicit return. R automatically updates the list like object 'output'
#    print("inside XVAR")
    if (is.null(input$the.file)){
      return()
    }
    # Call the ReadData method to read the field
    the.data <<- ReadData(input$the.file$datapath)
#    column.names <- colnames(the.data)
    
    # built in function to create a box with choices to select from
#    selectInput("Xvar", "Choose X variable", choices = c(" ", column.names), selected = colnames(the.data)[[2]])
    
#   
    
  })
  
  output$choose_Yvar <- renderUI({
    # unnamed function to create 'Choose Y variable' widget
    #
    # Args:
    #   Takes no arguments
    #
    # Returns:
    #   No explicit return. R automatically updates the list like object 'output'
#     input$the.file
#     update.file()
    if (is.null(input$the.file))
      return()
    if (is.null(the.data)) { 
      choice.temp <- c(" ", " ")
    } else { 
      choice.temp <- c(" ", colnames(the.data))
    }
    # built in function to create a box with choices to select from
    selectInput("Yvar", "Choose Y variable", choices = choice.temp, selected = colnames(the.data)[[3]])
  })
  
  output$choose_IDvar <- renderUI({
    # unnamed function to create 'Choose ID variable' widget
    #
    # Args:
    #   Takes no arguments
    #
    # Returns:
    #   No explicit return. R automatically updates the list like object 'output'
    
#    update.file()
    if (is.null(input$the.file))
      return()
    if (is.null(the.data)) { 
      choice.temp <- c(" ", " ")
    } else { 
      choice.temp <- c(" ", colnames(the.data))
    } 
    # built in function to create a box with choices to select from
    selectInput("IDvar", "Choose ID variable", choices = choice.temp, selected = colnames(the.data)[[4]] )
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
    
     summary(the.data)
    
  })


  output$ggvisplot_ui <- renderUI({

    flex.data <- reactive({
          print("main entry")
          print("enter")
          x.name <- input$Xvar
          print(input$Xvar)
          x.data <- the.data[, x.name]
          y.name <- input$Yvar
          y.data <- the.data[, y.name]
          id.name <- input$IDvar
          ID.t <- the.data[, id.name]
          new.data <- data.frame(x.data, y.data, ID.t)
      #    names(new.data) <- c(x.name, y.name, ID.t)
          print(new.data)
         })
        
    lb <- linked_brush(keys = 1:nrow(flex.data()), "deeppink")
          
    flex.data %>%
      ggvis(~x.data, ~y.data) %>%
      add_axis("x", title = "x") %>%
      add_axis("y", title = "y") %>%
      layer_points(fill := lb$fill, fill.brush := "mediumblue", fill := "deeppink") %>%
      lb$input()  %>%
      layer_points(fill := "mediumblue", data = reactive(flex.data()[flex.data()$ID.t %in%
                                                                      flex.data()[lb$selected(), ]$ID.t, ])) %>%
 #     set_options(width = 500, height = 350) %>%
      bind_shiny("ggvis_xy_plot")
      
    # Creating profile plot
     selected <- lb$selected
     new.data.selected <- reactive({
       #    if (!any(selected())) return(layer_lines())
       flex.data()[flex.data()$ID.t %in% flex.data()[lb$selected(), ]$ID.t, ]
     })
#  
    flex.data %>%
      ggvis(~x.data, ~y.data) %>%
      add_axis("x", title = "x") %>%
      add_axis("y", title = "y") %>%
      layer_points(fill := lb$fill, fill.brush := "mediumblue", fill := "deeppink") %>%
      group_by(ID.t) %>%
      layer_lines(stroke := "deeppink") %>% 
      lb$input()  %>%
      add_data(new.data.selected) %>%
      layer_points(fill := "mediumblue") %>%
      layer_lines(stroke := "mediumblue") %>%
      bind_shiny("ggvis_profile_plot")
    
  
    ggvisOutput("ggvis_xy_plot")
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
   #   eval(parse(text = model.name), my.env)
      if(class(eval(parse(text = model.name), my.env)) == "lme" | 
           class(eval(parse(text = model.name), my.env)) == "nlme" ){
        
        print("YES")
        diagnos.data.source <- eval(parse(text = model.name), my.env)
        res <- resid(diagnos.data.source)
        fit <- fitted(diagnos.data.source)
        pred <- predict(diagnos.data.source)
        df <- data.frame(res = res, fit = fit)
        
        # Building data frame for diagnostic plots
        upload.data <- reactive({
          print("main entry")
          print("enter")
          x.name <- input$Xvar
          print(input$Xvar)
          x.data <- the.data[, x.name]
          y.name <- input$Yvar
          y.data <- the.data[, y.name]
          id.name <- input$IDvar
          ID.t <- the.data[, id.name]
          new.data <- data.frame(x.data, y.data, ID.t, res, fit, pred)
          #    names(new.data) <- c(x.name, y.name, ID.t)
          print(new.data)
        })
        
        # Creating the Residuals vs Fitted values plot
        upload.data %>% 
          ggvis(~fit, ~res) %>% 
          add_axis("x", title = "Fitted values") %>%
          add_axis("y", title = "Standardized residuals") %>%
          layer_points() %>%
          set_options(width = 400, height = 300) %>%
          bind_shiny("res_fitted")
        
        # Creating the Predicted Values vs DV plot
        upload.data %>% 
          ggvis(~y.data, ~pred) %>% 
          add_axis("x", title = "DV(Y)") %>%
          add_axis("y", title = "Predicted values") %>%
          layer_points() %>%
          set_options(width = 400, height = 300) %>%
          bind_shiny("pred_dv")
        
        # Creating the Residuals vs DV plot
        upload.data %>% 
          ggvis(~y.data, ~res) %>% 
          add_axis("x", title = "DV(Y)") %>%
          add_axis("y", title = "Residuals") %>%
          layer_points() %>%
          set_options(width = 400, height = 300) %>%
          bind_shiny("res_dv")
        
        # Creating the Residuals vs TIME plot
        upload.data %>% 
          ggvis(~x.data, ~res) %>% 
          add_axis("x", title = "TIME(X)") %>%
          add_axis("y", title = "Residuals") %>%
          layer_points() %>%
          set_options(width = 400, height = 300) %>%
          bind_shiny("res_time")
        
        # Creating Predictions and DV vs TIME plot
        upload.data %>% 
          ggvis(~x.data, ~pred, fill = "predictions") %>% 
          add_axis("x", title = "TIME(X)") %>%
          add_axis("y", title = "Predictions and DV") %>%
          layer_points() %>%
          layer_points(data = upload.data, x = ~x.data, y = ~y.data, fill = "DV") %>%
          add_legend("fill") %>%
          set_options(width = 400, height = 300) %>%
          bind_shiny("pred_dv_time")
          
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
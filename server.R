library(shiny)
library(ggvis)
library(nlme)
library(shinyAce)
source("helper_functions.R")



shinyServer(function(input, output, session) {
  # server logic to create all the widgets and functionality of the app
  
  my.env <<- new.env()
  
  theData <- reactive({
    infile <- input$the.file        
    if (is.null(infile)) {
      the.data <<- Theoph
      return(NULL)
    }
    dat <- read.csv(infile$datapath, header = T)
    the.data <<- read.csv(infile$datapath, header = T)
    dat        
  })

  observe({
    data <- theData()
    updateSelectInput(session, 'Xvar', choices = names(data))
    updateSelectInput(session, 'Yvar', choices = names(data))
    updateSelectInput(session, 'IDvar', choices = names(data))
  })
    
  output$summary <- renderPrint({
    
     summary(the.data)
    
  })

  flex.data <- reactive({
    data <- isolate(theData())
    
    if((input$Xvar=="Subject" | input$Xvar=="Wt"| input$Xvar=="Dose"| input$Xvar=="Time"| input$Xvar=="conc")
      && (input$Yvar=="Subject" | input$Yvar=="Wt"| input$Yvar=="Dose"| input$Yvar=="Time"| input$Yvar=="conc") 
      && (input$IDvar=="Subject" | input$IDvar=="Wt"| input$IDvar=="Dose"| input$IDvar=="Time"| input$IDvar=="conc")){
      
      if(is.null(data)){
        print("main entry")
        print("enter")
          x.name <- input$Xvar
           print(input$Xvar)
          x.data <- Theoph[, x.name]
                  y.name <- input$Yvar
                  y.data <- Theoph[, y.name]
                  id.name <- input$IDvar
                  ID.t <- Theoph[, id.name]
        data <- data.frame(x.data, y.data, ID.t)
        #    names(new.data) <- c(x.name, y.name, ID.t)
     #   print(new.data)
      }
      } else {
        print("main entry1")
        print("enter1")
        x.name <- input$Xvar
        print(input$Xvar)
        x.data <- data[, x.name]
        y.name <- input$Yvar
        y.data <- data[, y.name]
        id.name <- input$IDvar
        ID.t <- data[, id.name]
        data <- data.frame(x.data, y.data, ID.t)
        #    names(new.data) <- c(x.name, y.name, ID.t)
      #  print(new.data)
      }
      data
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
    set_options(width = 500, height = 350) %>%
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
    set_options(width = 500, height = 350) %>%
    bind_shiny("ggvis_profile_plot")

  output$ggvisplot_ui <- renderUI({

    flex.data <- reactive({
      data <- isolate(theData())
      
      if(input$Xvar=="Wt" && input$Yvar=="Dose" && input$IDvar=="Time"){
        
        if(is.null(data)){
          print("main entry")
          print("enter")
       #   x.name <- input$Xvar
      #    print(input$Xvar)
       #   x.data <- Theoph[, x.name]
#           y.name <- input$Yvar
#           y.data <- Theoph[, y.name]
#           id.name <- input$IDvar
#           ID.t <- Theoph[, id.name]
          new.data <- data.frame(x.data = 0, y.data = 0, ID.t = 0)
          #    names(new.data) <- c(x.name, y.name, ID.t)
          print(new.data)
        } else {
          print("main entry")
          print("enter")
          x.name <- input$Xvar
          print(input$Xvar)
          x.data <- data[, x.name]
          y.name <- input$Yvar
          y.data <- data[, y.name]
          id.name <- input$IDvar
          ID.t <- data[, id.name]
          new.data <- data.frame(x.data, y.data, ID.t)
      #    names(new.data) <- c(x.name, y.name, ID.t)
          print(new.data)
        }
        new.data
      }
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
      bind_shiny("ggvis_xy_plot", "ggvis_xy_ui")
      
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
          # Creating a data frame for adding the residual = 0 line
          max_x <- max(df$fit)
          min_x <- min(df$fit)
          dat <- data.frame(fit = c(min_x, max_x), res = c(0, 0))
        
        upload.data %>% 
          ggvis(~fit, ~res) %>% 
          add_axis("x", title = "Fitted values") %>%
          add_axis("y", title = "Standardized residuals") %>%
          layer_points() %>%
          set_options(width = 400, height = 300) %>%
          layer_paths(stroke := "red", data = dat) %>%
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
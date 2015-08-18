library(shiny)
library(ggvis)
library(nlme)
library(shinyAce)
source("helper_functions.R")

shinyUI(fluidPage(
  headerPanel("Prototype 1"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("read_file"),
      br(),
      uiOutput("choose_Xvar"),
      uiOutput("choose_Yvar"),
      uiOutput("choose_IDvar"),
      uiOutput("choose_COVvar"),
      uiOutput("choose_COVn"),
      br()
    ),
    
  #   sidebarPanel(
  #     radioButtons("PlotMethod", label=h4("Plot type"),c("XY Scatter plot","profile plot"))
  #   ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
  #     ggvisOutput("ggp"),
  # #  ggvisOutput("ggp1"),
  #     plotOutput("plot")
        tabPanel("Plot", uiOutput("plot_type"), br(), htmlOutput("plot")),
#        tabPanel("Plot", htmlOutput("plot")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Model fitting", 
                 bootstrapPage(div(
                   class="container-fluid",
                   div(class="row-fluid",
                       div(class="span6",
                           h2("Source Code"),  
                           aceEditor("code", mode="r", value="text <- 10 
text"),
                           actionButton("eval", "Evaluate")
                       ),
                       div(class="span6",
                           h2("Output"),
                           verbatimTextOutput("output")
                       )
                   )
                )
                   
  #      htmlOutput("plot1")  
            )
          ),
       tabPanel("Diagnostic plots",
                uiOutput("diagnose")
                   
                )
        )
      )
    )
))
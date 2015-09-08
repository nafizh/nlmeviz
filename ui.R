library(shiny)
library(ggvis)
library(nlme)
library(shinyAce)
source("helper_functions.R")


shinyUI(fluidPage(
  headerPanel("Prototype 1"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput('the.file', 'Read Data File', accept = c('.csv', '.txt', '.sim', '.dat')),
      selectInput("Xvar", "Choose X(TIME) variable", choices = colnames(Theoph), selected = colnames(Theoph)[[2]] ),
      selectInput("Yvar", "Choose Y(DV) variable", choices = colnames(Theoph), selected = colnames(Theoph)[[3]] ),
      selectInput("IDvar", "Choose ID variable", choices = colnames(Theoph), selected = colnames(Theoph)[[4]] ),
      br()
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("Data Exploration",
                 br(),
                 br(),
                 fluidRow(
                   column(3,
                          ggvisOutput("ggvis_xy_plot")),
                   column(3, offset = 3,
                          ggvisOutput("ggvis_profile_plot"))
                 )
         ),

        tabPanel("Summary", verbatimTextOutput("summary")),
        
        # Inserting the R Editor
        tabPanel("Model fitting", 
                 bootstrapPage(div(
                   class="container-fluid",
                   div(class="row-fluid",
                       div(class="span6",
                           h2("Source Code"),  
                           aceEditor("code", mode="r", value="# The output section will not show anything unless you use print  
# If you write the name of a variable or a model, output will only show the last one.
# There is a sample model below that uses the Theoph data set, If you click Evaluate, you will see 
# diagnostic plots being generated at the next tab
# You will also see the model output at the output section, thanks to nlme.
# When fitting a new model, use \"the.data\" as the value of the data parameter.
# the.data refers to the dataset you have uploaded into nlmeviz.
library(nlme)
fm1Theo.lis <- nlsList(conc ~ SSfol(Dose, Time, lKe, lKa, lCl), data = the.data)
fm1Theo.nlme <- nlme(fm1Theo.lis)
fm1Theo.nlme"),
                           actionButton("eval", "Evaluate")
                       ),
                       div(class="span6",
                           h2("Output"),
                           verbatimTextOutput("output")
                       )
                     )
                   )  
                 )
               ),
       tabPanel("Diagnostic plots",
                uiOutput("diagnose"),
                br(),
                fluidRow(
                  column(3,
                         ggvisOutput("res_fitted")),
                  column(3, offset = 2,
                         ggvisOutput("pred_dv"))
                ),
                fluidRow(
                  column(3,
                         ggvisOutput("res_dv")),
                  column(3, offset = 2,
                         ggvisOutput("res_time"))
                  ),
                fluidRow(
                  column(3,
                         ggvisOutput("pred_dv_time"))
                )
          )
        )
      )
    )
))
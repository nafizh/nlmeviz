library(shiny)
library(ggvis)
library(nlme)
library(shinyAce)
source("helper_functions.R")


the.data <<- ReadData("/Users/Nafiz/Dropbox/Google_Summer_of_Code_2015/nlmeviz/Orthodont.csv")

shinyUI(fluidPage(
  headerPanel("Prototype 1"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("Xvar", "Choose X(TIME) variable", choices = colnames(the.data), selected = colnames(the.data)[[2]] ),
      selectInput("Yvar", "Choose Y(DV) variable", choices = colnames(the.data), selected = colnames(the.data)[[3]] ),
      selectInput("IDvar", "Choose ID variable", choices = colnames(the.data), selected = colnames(the.data)[[4]] ),
      br()
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("Data Exploration",
                 br(),
                 tabsetPanel(type = "tabs",
                             tabPanel("XY Scatter Plot", 
                                      br(),
                                      htmlOutput("ggvisplot_ui")),
                             tabPanel("Profile plot",
                                      br(),
                                      ggvisOutput("ggvis_profile_plot"))
                             )),

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
# There is a model below, If you click Evaluate, you will see 
# diagnostic plots being generated at the next tab
# You will also see the model output at the output section, thanks to nlme  
library(nlme)
fm1Orth.lme <- lme(distance ~ I(age-11), data = Orthodont, random = ~I(age-11) | Subject)
fm1Orth.lme"),
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
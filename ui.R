library(ggvis)
source("helper_functions.R")

shinyUI(fluidPage(
  headerPanel("Prototype 1"),
  
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
  
  sidebarPanel(
    radioButtons("PlotMethod", label=h4("Plot type"),c("XY Scatter plot","profile plot"))
  ),
  
  mainPanel(
#     ggvisOutput("ggp"),
# #  ggvisOutput("ggp1"),
#     plotOutput("plot")
      htmlOutput("plot"),
      htmlOutput("plot1")  
  )
))
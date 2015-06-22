ReadData <- function(file.path) { 
  # Reads the file chosen by the user
  # Args:
  #   file.path: The path of the file the user chooses
  #
  # Returns:
  #   returns a data frame with all column names in uppercase and 
  
  temp.data <- read.csv(file.path, na.strings = ".")
  colnames(temp.data) <- toupper(colnames(temp.data))
  return(temp.data)
}

DrawScatterPlot <- function(the.data, X.name, Y.name, ID.name, x.lim, y.lim) {  
  X.t <- the.data[, X.name]
  Y.t <- the.data[, Y.name]
  ID.t <- the.data[, ID.name]
  new.data <- data.frame(X.t = X.t, Y.t = Y.t, ID.t = ID.t)
  
  print(head(new.data))
  print(nrow(new.data))
  
  lb <- linked_brush(keys = 1:nrow(new.data), "red")
  
  new.data %>%
    ggvis(~X.t, ~Y.t) %>%
    add_axis("x", title = X.name) %>%
    add_axis("y", title = Y.name) %>%
    layer_points(fill := lb$fill, fill.brush := "red") %>% # to make the selected point red
    lb$input()  %>%
    # to make the other points with the same id red
    layer_points(fill := "red", data = reactive(new.data[new.data$ID.t %in%
                                                           new.data[lb$selected(), ]$ID.t, ])) %>%
    set_options(width = 400, height = 300) %>%
    bind_shiny("ggvisplot")
}

DrawScatterPlotWithCovar <- function(the.data, X.name, Y.name, ID.name, x.lim, y.lim, 
                                     COV.name){
  X <- the.data[, X.name]
  Y <- the.data[, Y.name]
  ID <- the.data[, ID.name]
  COV <- the.data[, COV.name]
  new.data <- data.frame(X = X, Y = Y, ID = ID, COV = COV)
  
  print(head(new.data))
  print(nrow(new.data))
  
  new.data$id <- seq_len(nrow(new.data))
  lb <- linked_brush(keys = new.data$id, "red")
  
  print("Hello with COV")
  new.data %>%
    ggvis(~X, ~Y) %>%
    #     layer_points(fill := lb$fill, fill.brush := "red", size.brush := 400, size := input_slider(10, 400), 
    #                  opacity := input_slider(0, 1)) %>%
    add_axis("x", title = X.name) %>%
    add_axis("y", title = Y.name) %>%
    layer_points(fill := lb$fill, fill.brush := "red") %>%
    lb$input()  %>%
    layer_points(fill := "red", data = reactive(new.data[new.data$ID %in% new.data[lb$selected(),]$ID, ])) %>%
    #    bind_shiny("visplot1", "slider_ui")
    set_options(width = 400, height = 300) %>%
    bind_shiny("ggvisplot1")
  
  selected <- lb$selected
  new.data.selected <- reactive({
    new.data[selected(), ]
  })
  
  new.data %>% 
    ggvis(~COV) %>% layer_histograms() %>% 
    add_data(new.data.selected) %>%
    layer_histograms(fill := "#dd3333") %>%
    set_options(width = 400, height = 300) %>%
    add_axis("x", title = COV.name) %>%
    bind_shiny("ggvisplot_cov") 
}

DrawProfilePlot <- function(the.data, X.name, Y.name, ID.name, x.lim, y.lim){
  X.t <- the.data[, X.name]
  Y.t <- the.data[, Y.name]
  ID.t <- the.data[, ID.name]
  new.data <- data.frame(X.t = X.t, Y.t = Y.t, ID.t = ID.t)
  
  print(head(new.data))
  print(nrow(new.data))

  lb <- linked_brush(keys = 1:nrow(new.data), "red")
  
  new.data %>%
    ggvis(~X.t, ~Y.t) %>%
    add_axis("x", title = X.name) %>%
    add_axis("y", title = Y.name) %>%
    layer_points(fill := lb$fill, fill.brush := "red") %>%
    group_by(ID.t) %>%
    layer_lines() %>%
    lb$input()  %>%
    layer_points(fill := "red", data = reactive(new.data[new.data$ID.t %in% 
                                                              new.data[lb$selected(), ]$ID.t, ])) %>%
    layer_lines() %>%
    set_options(width = 400, height = 300) %>%
    bind_shiny("ggvisProfilePlot")
}
ReadPkPdData <- function(file.name) { 
  temp.data <- read.csv(file.name,na.strings=".")
  colnames(temp.data) <- toupper(colnames(temp.data))
  return(temp.data)
}

DrawScatterPlot <- function(orig.data, X.name, Y.name, ID.name, x.lim, y.lim) {  
  X.t <- orig.data[, X.name]
  Y.t <- orig.data[, Y.name]
  ID.t <- orig.data[, ID.name]
  new.data <- data.frame(X.t = X.t, Y.t = Y.t, ID.t = ID.t)
  
  print(head(new.data))
  print(nrow(new.data))
  
  new.data$id <- seq_len(nrow(new.data))
  lb <- linked_brush(keys = new.data$id, "red")
  
  new.data %>%
    ggvis(~X.t, ~Y.t) %>%
    #     layer_points(fill := lb$fill, fill.brush := "red", size.brush := 400, size := input_slider(10, 400), 
    #                  opacity := input_slider(0, 1)) %>%
    layer_points(fill := lb$fill, fill.brush := "red") %>%
    lb$input()  %>%
    layer_points(fill := "red", data = reactive(new.data[new.data$ID.t %in% new.data[lb$selected(),]$ID.t, ])) %>%
    #    bind_shiny("visplot1", "slider_ui")
    bind_shiny("ggvisplot")
}   
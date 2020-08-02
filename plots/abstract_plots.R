total_plot <- function(xval, yval, linecolor, gtitle, ytitle) {
  fdate <- format(xval, "%Y-%m-%d")
  
  export <- list(xval = fdate, 
                 yval = yval, 
                 linecolor = linecolor, 
                 gtitle = gtitle, 
                 ytitle = ytitle,
                 type = "total")
  return(export)
}

daily_plot <- function(xval, yval, barcolor, fillcolor, movingAverage, movingLineColor, gtitle, ytitle) {
  fdate <- format(xval, "%Y-%m-%d")
  
  export <- list(xval = fdate, 
                 yval = yval, 
                 barcolor = barcolor, 
                 fillcolor = fillcolor, 
                 movingAverage = movingAverage,
                 movingLineColor = movingLineColor,
                 gtitle = gtitle, 
                 ytitle = ytitle,
                 type = "daily")
  return(export)
}
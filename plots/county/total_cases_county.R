total_cases_plot <- function(ds) {
  
  county <- paste(ds$COUNTY[1], 'County')
  g_title <- paste('Total Confirmed COVID-19 Cases in ', county, ", TN", sep = '')
  
  fdate <- format(ds$DATE, '%Y-%m-%d')
  
  export <- list(xval = fdate, 
                 yval = ds$TOTAL_CASES, 
                 linecolor = 'rgb(0, 164, 179)', 
                 gtitle = g_title, 
                 ytitle = "Confirmed Cases",
                 type = "total")
  
  return(export)
}
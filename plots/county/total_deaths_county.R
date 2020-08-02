total_deaths_plot <- function(ds) {
  
  county <- paste(ds$COUNTY[1], 'County')
  g_title <- paste('Total Confirmed COVID-19 Deaths in ', county, ", TN", sep = '')
  
  fdate <- format(ds$DATE, '%Y-%m-%d')
  
  export <- list(xval = fdate, 
                 yval = ds$TOTAL_DEATHS, 
                 linecolor = 'rgb(255, 93, 87)', 
                 gtitle = g_title, 
                 ytitle = "Confirmed Deaths",
                 type = "total")
  
  return(export)
}
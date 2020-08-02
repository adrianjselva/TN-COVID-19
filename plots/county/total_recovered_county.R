total_recovered_plot <- function(ds) {
  
  county <- paste(ds$COUNTY[1], 'County')
  g_title <- paste('Total Recovered COVID-19 Cases in ', county, ", TN", sep = '')
  
  start_index <- first_nonzero_index(ds$TOTAL_RECOVERED)
  mod_rec <- ds$TOTAL_RECOVERED[start_index:length(ds$TOTAL_RECOVERED)]
  mod_date <- ds$DATE[start_index:length(ds$DATE)]
  
  fdate <- format(mod_date, '%Y-%m-%d')
  
  export <- list(xval = fdate, 
                 yval = mod_rec, 
                 linecolor = 'rgb(231, 235, 30)', 
                 gtitle = g_title, 
                 ytitle = "Recovered Cases",
                 type = "total")
  
  return(export)
}
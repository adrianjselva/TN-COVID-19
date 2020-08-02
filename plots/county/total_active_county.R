current_active_cases_plot <- function(ds) {
  
  county <- paste(ds$COUNTY[1], 'County')
  g_title <- paste('Active COVID-19 Cases in ', county, ", TN", sep = '')
  
  start_index <- first_nonzero_index(ds$TOTAL_ACTIVE)
  mod_active <- ds$TOTAL_ACTIVE[start_index:length(ds$TOTAL_ACTIVE)]
  mod_date <- ds$DATE[start_index:length(ds$DATE)]
  
  fdate <- format(mod_date, '%Y-%m-%d')
  
  export <- list(xval = fdate, 
                 yval = mod_active, 
                 linecolor = 'rgb(0, 219, 69)', 
                 gtitle = g_title, 
                 ytitle = "Active Cases",
                 type = "total")
  
  return(export)
}
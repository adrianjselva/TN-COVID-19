total_hospitalized_plot <- function(ds) {
  
  county <- paste(ds$COUNTY[1], 'County')
  g_title <- paste('Total COVID-19 Hospitalizations in ', county, ", TN", sep = '')
  
  start_index <- first_nonzero_index(ds$TOTAL_HOSPITALIZED)
  mod_hosp <- ds$TOTAL_HOSPITALIZED[start_index:length(ds$TOTAL_HOSPITALIZED)]
  mod_date <- ds$DATE[start_index:length(ds$DATE)]
  
  fdate <- format(mod_date, '%Y-%m-%d')
  
  export <- list(xval = fdate, 
                 yval = mod_hosp, 
                 linecolor = 'rgb(245, 99, 15)', 
                 gtitle = g_title, 
                 ytitle = "Hospitalized Cases",
                 type = "total")
  
  return(export)
}
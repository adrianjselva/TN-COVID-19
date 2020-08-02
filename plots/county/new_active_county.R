daily_active_cases_plot <- function(ds) {
  start_index <- first_nonzero_index(ds$NEW_ACTIVE)
  mod_new_active <- ds$NEW_ACTIVE[start_index:length(ds$NEW_ACTIVE)]
  mod_date <- ds$DATE[start_index:length(ds$DATE)]
  
  mov_avg <- seven_day_average(mod_new_active)
  
  county <- paste(ds$COUNTY[1], 'County')
  g_title <- paste('Daily Change of Active COVID-19 Cases in ', county, ", TN", sep = '')
  
  fdate <- format(mod_date, '%Y-%m-%d')
  
  export <- list(xval = fdate, 
                 yval = mod_new_active, 
                 barcolor = 'rgba(56, 209, 105, 0.5)', 
                 fillcolor = 'rgba(56, 209, 105, 0.25)', 
                 movingAverage = mov_avg,
                 movingLineColor = 'rgb(0, 219, 69)',
                 gtitle = g_title, 
                 ytitle = "Daily Change of Active Cases",
                 type = "daily")
  
  return(export)
}
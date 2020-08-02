daily_recovered_plot <- function(ds) {
  start_index <- first_nonzero_index(ds$NEW_RECOVERED)
  mod_daily_rec <- ds$NEW_RECOVERED[start_index:length(ds$NEW_RECOVERED)]
  mod_date <- ds$DATE[start_index:length(ds$DATE)]
  
  mov_avg <- seven_day_average(mod_daily_rec)
  
  county <- paste(ds$COUNTY[1], 'County')
  g_title <- paste('Daily Recovered COVID-19 Cases in ', county, ", TN", sep = '')
  
  fdate <- format(mod_date, '%Y-%m-%d')
  
  export <- list(xval = fdate, 
                 yval = mod_daily_rec, 
                 barcolor = 'rgba(197, 201, 22, 0.5)', 
                 fillcolor = 'rgba(197, 201, 22, 0.25)', 
                 movingAverage = mov_avg,
                 movingLineColor = 'rgb(231, 235, 30)',
                 gtitle = g_title, 
                 ytitle = "Daily Recovered Cases",
                 type = "daily")
  
  return(export)
}
daily_hospitalizations_plot <- function(ds) {
  start_index <- first_nonzero_index(ds$NEW_HOSPITALIZED)
  mod_daily_hosp <- ds$NEW_HOSPITALIZED[start_index:length(ds$NEW_HOSPITALIZED)]
  mod_date <- ds$DATE[start_index:length(ds$DATE)]
  
  mov_avg <- seven_day_average(mod_daily_hosp)
  
  county <- paste(ds$COUNTY[1], 'County')
  g_title <- paste('Daily COVID-19 Hospitalizations in ', county, ", TN", sep = '')
  
  fdate <- format(mod_date, '%Y-%m-%d')
  
  export <- list(xval = fdate, 
                 yval = mod_daily_hosp, 
                 barcolor = 'rgba(245, 136, 73, 0.5)', 
                 fillcolor = 'rgba(245, 136, 73, 0.25)', 
                 movingAverage = mov_avg,
                 movingLineColor = 'rgb(245, 99, 15)',
                 gtitle = g_title, 
                 ytitle = "Daily Hospitalizations",
                 type = "daily")
  
  return(export)
}
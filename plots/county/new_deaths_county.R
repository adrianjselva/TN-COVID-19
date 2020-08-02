daily_deaths_plot <- function(ds) {
  mov_avg <- seven_day_average(ds$NEW_DEATHS)
  
  county <- paste(ds$COUNTY[1], 'County')
  g_title <- paste('Daily Confirmed COVID-19 Deaths in ', county, ", TN", sep = '')
  
  fdate <- format(ds$DATE, '%Y-%m-%d')
  
  export <- list(xval = fdate, 
                 yval = ds$NEW_DEATHS, 
                 barcolor = 'rgba(255, 124, 120, 0.5)', 
                 fillcolor = 'rgba(255, 124, 120, 0.25)', 
                 movingAverage = mov_avg,
                 movingLineColor = 'rgb(255, 93, 87)',
                 gtitle = g_title, 
                 ytitle = "Daily Confirmed Deaths",
                 type = "daily")
  
  return(export)
}
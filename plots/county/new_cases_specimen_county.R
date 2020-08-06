daily_cases_specimen_plot <- function(ds) {
  mov_avg <- seven_day_average(ds$NEW_CASES)
  
  county <- paste(ds$COUNTY[1], 'County')
  g_title <- paste('Daily COVID-19 Cases by Specimen Collection Date in ', county, ", TN", sep = '')
  
  fdate <- format(ds$DATE, '%Y-%m-%d')
  
  export <- list(xval = fdate, 
                 yval = ds$NEW_CASES, 
                 barcolor = 'rgba(0, 182, 199, 0.5)', 
                 fillcolor = 'rgba(0, 182, 199, 0.25)', 
                 movingAverage = mov_avg,
                 movingLineColor = 'rgb(0, 164, 179)',
                 gtitle = g_title, 
                 ytitle = "New Cases (Specimen Collection Date)",
                 type = "specimen")
  
  return(export)
}
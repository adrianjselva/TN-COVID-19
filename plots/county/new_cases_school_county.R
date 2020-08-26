new_cases_school_county <- function(ds) {
  mov_avg <- seven_day_average(ds$NEW_CASES)
  
  fdate <- format(ds$DATE, '%Y-%m-%d')
  
  export <- list(xval = fdate, 
                 yval = ds$NEW_CASES, 
                 barcolor = 'rgba(0, 182, 199, 0.5)', 
                 fillcolor = 'rgba(0, 182, 199, 0.25)', 
                 movingAverage = mov_avg,
                 movingLineColor = 'rgb(0, 164, 179)',
                 type = "daily")
  
  return(export)
}
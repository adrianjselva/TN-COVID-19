daily_cases_specimen_state_plot <- function(ds) {
  nds <- create_state_specimen_collection(ds)
  
  mov_avg <- seven_day_average(nds$NEW_CASES)
  
  g_title <- paste('Daily COVID-19 Cases by Specimen Collection Date in Tennessee')
  
  fdate <- format(nds$DATE, '%Y-%m-%d')
  
  export <- list(xval = fdate, 
                 yval = nds$NEW_CASES, 
                 barcolor = 'rgba(0, 182, 199, 0.5)', 
                 fillcolor = 'rgba(0, 182, 199, 0.25)', 
                 movingAverage = mov_avg,
                 movingLineColor = 'rgb(0, 164, 179)',
                 gtitle = g_title, 
                 ytitle = "New Cases (Specimen Collection Date)",
                 type = "daily")
  
  return(export)
}
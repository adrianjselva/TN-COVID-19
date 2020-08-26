total_cases_school_county <- function(ds) {
  fdate <- format(ds$DATE, '%Y-%m-%d')
  
  export <- list(xval = fdate, 
                 yval = ds$CASE_COUNT, 
                 linecolor = 'rgb(0, 164, 179)', 
                 type = "total")
  
  return(export)
}
current_active_cases_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(names = c(NA), current_active_cases = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    names <- cty$COUNTY[1]
    current_active_cases <- cty$TOTAL_ACTIVE[length(cty$TOTAL_ACTIVE)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(names, current_active_cases)
    } else {
      df[nrow(df) + 1,] <- c(names, current_active_cases)
    }
    
  }
  
  df$current_active_cases <- as.numeric(df$current_active_cases)
  
  export <- list(counties = df$names, 
                 z = df$current_active_cases, 
                 type = "cmap", 
                 mtitle = "Active COVID-19 Cases",
                 hovtext = "Active cases:",
                 col1 = 'rgb(255, 255, 255)',
                 col2 = 'rgb(158, 255, 182)',
                 col3 = 'rgb(105, 255, 142)',
                 col4 = 'rgb(50, 201, 88)',
                 col5 = 'rgb(0, 138, 34)')
  
  return(export)
}
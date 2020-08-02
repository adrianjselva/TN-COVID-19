daily_cases_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(names = c(NA), new_cases = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    name <- cty$COUNTY[1]
    new_cases <- cty$NEW_CASES[length(cty$NEW_CASES)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(name, new_cases)
    } else {
      df[nrow(df) + 1,] <- c(name, new_cases)
    }
    
  }
  
  df$new_cases <- as.numeric(df$new_cases)
  
  export <- list(counties = df$names, 
                 z = df$new_cases, 
                 type = "cmap", 
                 mtitle = paste("Daily COVID-19 Cases:", mcurr_date),
                 hovtext = "New cases:",
                 col1 = 'rgb(255, 255, 255)',
                 col2 = 'rgb(164, 225, 252)',
                 col3 = 'rgb(105, 201, 245)',
                 col4 = 'rgb(3, 175, 255)',
                 col5 = 'rgb(0, 56, 120)')
  
  return(export)
}

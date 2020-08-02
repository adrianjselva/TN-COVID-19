daily_hospitalizations_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(names = c(NA), new_hospitalizations = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    names <- cty$COUNTY[1]
    new_hospitalizations <- cty$NEW_HOSPITALIZED[length(cty$NEW_HOSPITALIZED)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(names, new_hospitalizations)
    } else {
      df[nrow(df) + 1,] <- c(names, new_hospitalizations)
    }
    
  }
  
  df$new_hospitalizations <- as.numeric(df$new_hospitalizations)
  
  export <- list(counties = df$names, 
                 z = df$new_hospitalizations, 
                 type = "cmap", 
                 mtitle = paste("Daily COVID-19 Hospitalizations:", mcurr_date),
                 hovtext = "New hospitalizations:",
                 col1 = 'rgb(255, 255, 255)',
                 col2 = 'rgb(255, 204, 163)',
                 col3 = 'rgb(255, 175, 110)',
                 col4 = 'rgb(222, 134, 62)',
                 col5 = 'rgb(191, 86, 0)')
  
  return(export)
}
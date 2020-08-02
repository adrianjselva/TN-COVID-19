daily_deaths_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(names = c(NA), new_deaths = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    names <- cty$COUNTY[1]
    new_deaths <- cty$NEW_DEATHS[length(cty$NEW_DEATHS)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(names, new_deaths)
    } else {
      df[nrow(df) + 1,] <- c(names, new_deaths)
    }
    
  }
  
  df$new_deaths <- as.numeric(df$new_deaths)
  
  export <- list(counties = df$names, 
                 z = df$new_deaths, 
                 type = "cmap", 
                 mtitle = paste("Daily COVID-19 Deaths:", mcurr_date),
                 hovtext = "New deaths:",
                 col1 = 'rgb(255, 255, 255)',
                 col2 = 'rgb(252, 165, 164)',
                 col3 = 'rgb(240, 101, 101)',
                 col4 = 'rgb(199, 28, 28)',
                 col5 = 'rgb(148, 0, 0)')
  
  return(export)
}

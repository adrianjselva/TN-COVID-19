daily_recovered_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(names = c(NA), daily_recovered = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    names <- cty$COUNTY[1]
    daily_recovered <- cty$NEW_RECOVERED[length(cty$NEW_RECOVERED)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(names, daily_recovered)
    } else {
      df[nrow(df) + 1,] <- c(names, daily_recovered)
    }
    
  }
  
  df$daily_recovered <- as.numeric(df$daily_recovered)
  
  export <- list(counties = df$names, 
                 z = df$daily_recovered, 
                 type = "cmap", 
                 mtitle = paste("Daily COVID-19 Recoveries:", mcurr_date),
                 hovtext = "New recoveries:",
                 col1 = 'rgb(255, 255, 255)',
                 col2 = 'rgb(250, 255, 156)',
                 col3 = 'rgb(248, 255, 102)',
                 col4 = 'rgb(233, 242, 44)',
                 col5 = 'rgb(165, 173, 0)')
  
  return(export)
}

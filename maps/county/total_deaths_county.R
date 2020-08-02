total_deaths_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(names = c(NA), total_deaths = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    names <- cty$COUNTY[1]
    total_deaths <- cty$TOTAL_DEATHS[length(cty$TOTAL_DEATHS)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(names, total_deaths)
    } else {
      df[nrow(df) + 1,] <- c(names, total_deaths)
    }
    
  }
  
  df$total_deaths <- as.numeric(df$total_deaths)
  
  export <- list(counties = df$names, 
                 z = df$total_deaths, 
                 type = "cmap", 
                 mtitle = "Total COVID-19 Deaths",
                 hovtext = "Total deaths:",
                 col1 = 'rgb(255, 255, 255)',
                 col2 = 'rgb(255, 179, 179)',
                 col3 = 'rgb(240, 101, 101)',
                 col4 = 'rgb(199, 28, 28)',
                 col5 = 'rgb(148, 0, 0)')
  
  return(export)
}
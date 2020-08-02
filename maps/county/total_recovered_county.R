total_recovered_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(names = c(NA), total_recovered = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    names <- cty$COUNTY[1]
    total_recovered <- cty$TOTAL_RECOVERED[length(cty$TOTAL_RECOVERED)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(names, total_recovered)
    } else {
      df[nrow(df) + 1,] <- c(names, total_recovered)
    }
    
  }
  
  df$total_recovered <- as.numeric(df$total_recovered)
  
  export <- list(counties = df$names, 
                 z = df$total_recovered, 
                 type = "cmap", 
                 mtitle = "Total COVID-19 Recoveries",
                 hovtext = "Recovered cases:",
                 col1 = 'rgb(255, 255, 255)',
                 col2 = 'rgb(250, 255, 156)',
                 col3 = 'rgb(248, 255, 102)',
                 col4 = 'rgb(233, 242, 44)',
                 col5 = 'rgb(165, 173, 0)')
  
  return(export)
}
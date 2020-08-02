total_hospitalizations_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(names = c(NA), total_hospitalizations = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    names <- cty$COUNTY[1]
    total_hospitalizations <- cty$TOTAL_HOSPITALIZED[length(cty$TOTAL_HOSPITALIZED)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(names, total_hospitalizations)
    } else {
      df[nrow(df) + 1,] <- c(names, total_hospitalizations)
    }
    
  }
  
  df$total_hospitalizations <- as.numeric(df$total_hospitalizations)
  
  export <- list(counties = df$names, 
                 z = df$total_hospitalizations, 
                 type = "cmap", 
                 mtitle = "Total COVID-19 Hospitalizations",
                 hovtext = "Total hospitalizations:",
                 col1 = 'rgb(255, 255, 255)',
                 col2 = 'rgb(255, 204, 163)',
                 col3 = 'rgb(255, 175, 110)',
                 col4 = 'rgb(222, 134, 62)',
                 col5 = 'rgb(191, 86, 0)')
  
  return(export)
}
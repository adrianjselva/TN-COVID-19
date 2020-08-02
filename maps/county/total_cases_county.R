total_cases_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(names = c(NA), total_cases = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    names <- cty$COUNTY[1]
    total_cases <- cty$TOTAL_CASES[length(cty$TOTAL_CASES)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(names, total_cases)
    } else {
      df[nrow(df) + 1,] <- c(names, total_cases)
    }
    
  }
  
  df$total_cases <- as.numeric(df$total_cases)
  
  export <- list(counties = df$names, 
                 z = df$total_cases, 
                 type = "cmap", 
                 mtitle = "Total COVID-19 Cases",
                 hovtext = "Total cases:",
                 col1 = 'rgb(255, 255, 255)',
                 col2 = 'rgb(164, 225, 252)',
                 col3 = 'rgb(105, 201, 245)',
                 col4 = 'rgb(3, 175, 255)',
                 col5 = 'rgb(0, 56, 120)')
  
  return(export)
}
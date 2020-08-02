total_tests_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(names = c(NA), total_tests = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    names <- cty$COUNTY[1]
    total_tests <- cty$TOTAL_TESTS[length(cty$TOTAL_TESTS)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(names, total_tests)
    } else {
      df[nrow(df) + 1,] <- c(names, total_tests)
    }
    
  }
  
  df$total_tests <- as.numeric(df$total_tests)
  
  export <- list(counties = df$names, 
                 z = df$total_tests, 
                 type = "cmap", 
                 mtitle = "Total COVID-19 Tests",
                 hovtext = "Total tests performed:",
                 col1 = 'rgb(255, 255, 255)',
                 col2 = 'rgb(232, 171, 255)',
                 col3 = 'rgb(219, 122, 255)',
                 col4 = 'rgb(160, 50, 201)',
                 col5 = 'rgb(114, 2, 156)')
  
  return(export)
}
daily_testing_data_plot <- function(ds) {
  
  county <- paste(ds$COUNTY[1], 'County')
  g_title <- paste('Daily Testing Data for ', county, ", TN", sep = '')
  
  start_index <- first_nonzero_index(ds$NEW_TESTS)
  mod_date_range <- ds$DATE[start_index:length(ds$DATE)]
  mod_test_range <- ds$NEW_TESTS[start_index:length(ds$NEW_TESTS)]
  mod_pos_range <- ds$NEW_POS_TESTS[start_index:length(ds$NEW_POS_TESTS)]
  
  y2 <- list(
    overlaying = "y",
    side = "right",
    title = "Percent Positive (7-day Average)",
    rangemode = 'tozero',
    showgrid = FALSE
  )
  
  fdate <- format(mod_date_range, '%Y-%m-%d')
  
  percent_positve <- (mod_pos_range / mod_test_range) * 100
  percent_positve <- seven_day_average(percent_positve)
  percent_positve <- round(percent_positve, 1)
  percent_positve[which(!is.finite(percent_positve))] <- 0.0
  percent_positve[which(percent_positve < 0.0)] <- 0.0
  
  result <- plot_ly(data = ds, showlegend = TRUE, hoverinfo = 'skip') 
  result <- result %>% add_trace(x = mod_date_range, 
                                 y = mod_test_range, 
                                 type = 'bar',
                                 name = 'Total Tests',
                                 hoverinfo = 'x+y',
                                 color = I('rgba(206, 162, 219, 1)'))
  result <- result %>% add_trace(x = mod_date_range, 
                                 y = mod_pos_range, 
                                 type = 'bar', 
                                 name = 'Positive Tests',
                                 hoverinfo = 'x+y',
                                 color = I('rgba(0, 182, 199, 1)'))
  result <- result %>% add_trace(x = mod_date_range,
                                 y = percent_positve,
                                 type = 'scatter',
                                 mode = 'lines',
                                 name = 'Positive (%)',
                                 yaxis = "y2",
                                 hoverinfo = 'x+y',
                                 color = I('rgba(191, 23, 23, 1)'))
  result <- result %>% layout(xaxis = list(title = 'Date'),
                              yaxis = list(title = 'Daily Tests'),
                              yaxis2 = y2,
                              barmode = 'overlay',
                              title = g_title,
                              legend = list(y = -.3, orientation = 'h'),
                              margin = list(r = 45))
  result <- result %>% config(displayModeBar = FALSE, displaylogo = FALSE, responsive = TRUE)
  
  export <- list(xval = fdate, 
                 totalTestVal = mod_test_range,
                 positiveVals = mod_pos_range,
                 percentPositive = percent_positve,
                 gtitle = g_title,
                 type = "testing")
  
  return(export)
}
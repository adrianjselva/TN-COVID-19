testing_data_state_plot <- function() {
  g_title <- 'Daily Testing Data for Tennessee'
  
  start_index <- first_nonzero_index(daily_case_info_dataset$NEW_TESTS)
  mod_date_range <- daily_case_info_dataset$DATE[start_index:length(daily_case_info_dataset$DATE)]
  mod_test_range <- daily_case_info_dataset$NEW_TESTS[start_index:length(daily_case_info_dataset$NEW_TESTS)]
  mod_pos_range <- daily_case_info_dataset$NEW_POS_TESTS[start_index:length(daily_case_info_dataset$NEW_POS_TESTS)]
  
  fdate <- format(mod_date_range, '%Y-%m-%d')
  
  percent_positve <- (mod_pos_range / mod_test_range) * 100
  percent_positve <- seven_day_average(percent_positve)
  percent_positve <- round(percent_positve, 1)
  percent_positve[which(!is.finite(percent_positve))] <- 0.0
  percent_positve[which(percent_positve < 0.0)] <- 0.0 
  
  export <- list(xval = fdate, 
                 totalTestVal = mod_test_range,
                 positiveVals = mod_pos_range,
                 percentPositive = percent_positve,
                 gtitle = g_title,
                 type = "testing")
  
  return(export)
}
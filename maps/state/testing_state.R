testing_data_state_map <- function() {
  return(state_map(tail(daily_case_info_dataset$TOTAL_TESTS, n = 1),
                   "Total tests:",
                   'rgb(219, 122, 255)',
                   "Total COVID-19 Tests Performed"))
}
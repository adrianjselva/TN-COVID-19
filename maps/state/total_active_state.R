active_cases_state_map <- function() {
  return(state_map(tail(daily_case_info_dataset$TOTAL_ACTIVE, n = 1),
                   "Active cases:",
                   'rgb(105, 255, 142)',
                   "Active COVID-19 Cases"))
}
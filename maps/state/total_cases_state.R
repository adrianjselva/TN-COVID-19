total_cases_state_map <- function() {
  return(state_map(tail(daily_case_info_dataset$TOTAL_CASES, n = 1),
                   "Total cases:",
                   'rgb(105, 201, 245)',
                   "Total COVID-19 Cases"))
}
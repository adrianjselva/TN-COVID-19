daily_cases_state_map <- function() {
  return(state_map(tail(daily_case_info_dataset$NEW_CASES, n = 1),
                   "New cases:",
                   'rgb(105, 201, 245)',
                   paste("New COVID-19 Cases:", mcurr_date)))
}
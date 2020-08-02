daily_active_cases_state_map <- function() {
  return(state_map(tail(daily_case_info_dataset$NEW_ACTIVE, n = 1),
                   "Net change of active cases:",
                   'rgb(105, 255, 142)',
                   paste("New Active COVID-19 Cases:", mcurr_date)))
}
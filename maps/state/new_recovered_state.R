daily_recovered_state_map <- function() {
  return(state_map(tail(daily_case_info_dataset$NEW_RECOVERED, n = 1),
                   "New recoveries:",
                   'rgb(248, 255, 102)',
                   paste("New COVID-19 Recoveries:", mcurr_date)))
}
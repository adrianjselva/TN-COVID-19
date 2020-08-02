daily_deaths_state_map <- function() {
  return(state_map(tail(daily_case_info_dataset$NEW_DEATHS, n = 1),
                   "New deaths:",
                   'rgb(240, 101, 101)',
                   paste("New COVID-19 Deaths:", mcurr_date)))
}
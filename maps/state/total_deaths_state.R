total_deaths_state_map <- function() {
  return(state_map(tail(daily_case_info_dataset$TOTAL_DEATHS, n = 1),
                   "New cases:",
                   'rgb(240, 101, 101)',
                   "Total COVID-19 Deaths"))
}

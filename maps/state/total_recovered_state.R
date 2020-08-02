total_recovered_state_map <- function() {
  return(state_map(tail(daily_case_info_dataset$TOTAL_RECOVERED, n = 1),
                   "Total recovered:",
                   'rgb(248, 255, 102)',
                   "Total COVID-19 Recoveries"))
}

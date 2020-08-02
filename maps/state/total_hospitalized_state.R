total_hospitalizations_state_map <- function() {
  return(state_map(tail(daily_case_info_dataset$TOTAL_HOSP, n = 1),
                   "Total hospitalized:",
                   'rgb(255, 175, 110)',
                   "Total COVID-19 Hospitalizations"))
}
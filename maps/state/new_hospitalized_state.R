daily_hospitalizations_state_map <- function() {
  return(state_map(tail(daily_case_info_dataset$NEW_HOSP, n = 1),
                   "New hospitalizations:",
                   'rgb(255, 175, 110)',
                   paste("New COVID-19 Hospitalizations:", mcurr_date)))
}
total_cases_state_plot <- function() {
  return(total_plot(daily_case_info_dataset$DATE,
                    daily_case_info_dataset$TOTAL_CASES,
                    'rgb(0, 164, 179)',
                    "Total COVID-19 Cases in Tennessee",
                    "Total cases"))
}
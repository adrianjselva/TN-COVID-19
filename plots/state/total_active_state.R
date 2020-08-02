active_cases_state_plot <- function() {
  return(total_plot(daily_case_info_dataset$DATE,
                    daily_case_info_dataset$TOTAL_ACTIVE,
                    'rgb(0, 219, 69)',
                    'Active COVID-19 Cases in Tennessee',
                    "Active cases"))
}
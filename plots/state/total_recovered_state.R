total_recovered_state_plot <- function() {
  return(total_plot(daily_case_info_dataset$DATE,
                    daily_case_info_dataset$TOTAL_RECOVERED,
                    'rgb(231, 235, 30)',
                    'Total Recovered COVID-19 Cases in Tennessee',
                    'Total recovered cases'))
}
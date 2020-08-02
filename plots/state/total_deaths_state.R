total_deaths_state_plot <- function() {
  return(total_plot(daily_case_info_dataset$DATE,
                    daily_case_info_dataset$TOTAL_DEATHS,
                    'rgb(255, 93, 87)',
                    "Total COVID-19 Deaths in Tennessee",
                    "Total deaths"))
}
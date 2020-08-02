total_hospitalizations_state_plot <- function() {
  return(total_plot(daily_case_info_dataset$DATE,
                    daily_case_info_dataset$TOTAL_HOSP,
                    'rgb(245, 99, 15)',
                    'Total COVID-19 Hospitalizations in Tennessee',
                    'Total hospitalizations'))
}
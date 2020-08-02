daily_hospitalizations_state_plot <- function() {
  mov_avg <- seven_day_average(daily_case_info_dataset$NEW_HOSP)
  
  return(daily_plot(daily_case_info_dataset$DATE,
                    daily_case_info_dataset$NEW_HOSP,
                    'rgba(245, 136, 73, 0.5)',
                    'rgba(245, 136, 73, 0.25)',
                    mov_avg,
                    'rgb(245, 99, 15)',
                    "Daily COVID-19 Hospitalizations in Tennessee",
                    "Daily hospitalizations"))
}
daily_deaths_state_plot <- function() {
  mov_avg <- seven_day_average(daily_case_info_dataset$NEW_DEATHS)
  
  return(daily_plot(daily_case_info_dataset$DATE,
                    daily_case_info_dataset$NEW_DEATHS,
                    'rgba(255, 124, 120, 0.5)',
                    'rgba(255, 124, 120, 0.25)',
                    mov_avg,
                    'rgb(255, 93, 87)',
                    "Daily COVID-19 Deaths in Tennessee",
                    "Daily deaths"))
}
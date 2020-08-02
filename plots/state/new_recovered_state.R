daily_recovered_state_plot <- function() {
  mov_avg <- seven_day_average(daily_case_info_dataset$NEW_RECOVERED)
  
  return(daily_plot(daily_case_info_dataset$DATE,
                    daily_case_info_dataset$NEW_RECOVERED,
                    'rgba(197, 201, 22, 0.5)',
                    'rgba(197, 201, 22, 0.25)', 
                    mov_avg,
                    'rgb(231, 235, 30)',
                    "Daily Recovered COVID-19 Cases in Tennessee",
                    "Daily recovered cases"))
}
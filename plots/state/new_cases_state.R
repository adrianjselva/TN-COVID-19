daily_cases_state_plot <- function() {
  mov_avg <- seven_day_average(daily_case_info_dataset$NEW_CASES)
  
  return(daily_plot(daily_case_info_dataset$DATE,
                    daily_case_info_dataset$NEW_CASES,
                    'rgba(0, 182, 199, 0.5)',
                    'rgba(0, 182, 199, 0.25)',
                    mov_avg,
                    'rgb(0, 164, 179)',
                    "Daily COVID-19 Cases in Tennessee",
                    "Daily cases"))
}
daily_active_cases_state_plot <- function() {
  mov_avg <- seven_day_average(daily_case_info_dataset$NEW_ACTIVE)
  
  return(daily_plot(daily_case_info_dataset$DATE,
                    daily_case_info_dataset$NEW_ACTIVE,
                    'rgba(56, 209, 105, 0.5)',
                    'rgba(56, 209, 105, 0.25)', 
                    mov_avg,
                    'rgb(0, 219, 69)',
                    "Daily Change of Active COVID-19 Cases in Tennessee",
                    "Daily change of active cases"))
}
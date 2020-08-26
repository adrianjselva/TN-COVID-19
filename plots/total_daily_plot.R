total_daily_plot <- function(total, daily, xtitle, shapes) {
  cases_obj <- list(type = "total_daily",
                    daily = daily, 
                    total = total,
                    xtitle = xtitle,
                    shapes = shapes)
  
  return(cases_obj)
} 
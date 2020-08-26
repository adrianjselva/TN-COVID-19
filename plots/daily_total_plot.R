daily_total_plot <- function(daily, total, xtitle, shapes) {
  cases_obj <- list(type = "daily_total",
                    daily = daily, 
                    total = total,
                    xtitle = xtitle,
                    shapes = shapes)
  
  return(cases_obj)
} 
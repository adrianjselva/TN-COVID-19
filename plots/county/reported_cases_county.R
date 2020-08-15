reported_cases_plot_county <- function(daily, total) {
  cases_obj <- list(type = "reported",
                    daily = daily, 
                    total = total,
                    xtitle = "Reporting Date",
                    ytitle = "Cases",
                    shapes = list())
  
  return(cases_obj)
}
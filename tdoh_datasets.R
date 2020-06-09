#!/bin/env Rscript

######################################
# Libraries
######################################

library(readxl)
library(rjson)
library(plotly)

######################################
# Functions
######################################

add_xlsx <- function(file_name) {
  return(paste(file_name, ".xlsx", sep = ""))
}

temp_path <- function(file_name) {
  return(paste("temp", file_name, sep = "/"))
}

convert_date <- function(df, col_list) {
  df[[col_list[1]]] <- as.Date(df[[col_list[1]]])
  return(df)
}

load_file <- function(path, c_type) {
  f <- read_excel(path, col_types = c_type)
  f <- convert_date(f, colnames(f))
  f[is.na(f)] <- 0
  return(f)
}

recent_date <- function(df) {
  return(df[nrow(df),1][[1]])
}

all_same_date <- function(lof_df) {
  curr_dates <- lapply(lof_df, recent_date)
  return(length(unique(curr_dates)) == 1)
}

curr_dataset_date <- function(lof_df) {
  curr_dates <- lapply(lof_df, recent_date)
  return(curr_dates[1][[1]])
}

dataset_dir <- function() {
  dirs <- list.files("datasets")
  
  if(length(dirs) == 0) {
    return(NULL)
  }
  
  dirs <- lapply(dirs, function(x) {as.Date(x, format = "%m-%d-%Y")})
  dirs <- do.call(c, dirs)
  return(dirs)
}

counties_dir <- function() {
  dirs <- list.files("counties")
  
  if(length(dirs) == 0) {
    return(NULL)
  }
  
  dirs <- lapply(dirs, function(x) {as.Date(x, format = "%m-%d-%Y")})
  dirs <- do.call(c, dirs)
  return(dirs)
}

plots_dir <- function() {
  dirs <- list.files('plots')
  
  if(length(dirs) == 0) {
    return(NULL)
  }
  
  dirs <- lapply(dirs, function(x) {as.Date(x, format = "%m-%d-%Y")})
  dirs <- do.call(c, dirs)
  return(dirs)
}

maps_dir <- function() {
  dirs <- list.files('maps')
  
  if(length(dirs) == 0) {
    return(NULL)
  }
  
  dirs <- lapply(dirs, function(x) {as.Date(x, format = "%m-%d-%Y")})
  dirs <- do.call(c, dirs)
  return(dirs)
}

formatted_date <- function(d) {
  return(format(d, format = "%m-%d-%Y"))
}

dataset_path <- function(d) {
  return(paste("datasets", formatted_date(d), sep = "/"))
}

counties_path <- function(d) {
  return(paste("counties", formatted_date(d), sep = "/"))
}

plot_path <- function(d) {
  return(paste("plots", formatted_date(d), sep = "/"))
}

map_path <- function(d) {
  return(paste("maps", formatted_date(d), sep = "/"))
}

del_temp <- function() {
  unlink("temp", recursive = TRUE)
}

split_by_date <- function(dataset) {
  date_list <- list() 
  
  for(date_r in unique(dataset[[1]])) {
    d_data <- list(dataset[dataset[[1]] == date_r,])
    
    date_list <- append(date_list, d_data)
  }
  
  return(date_list)
}

split_by_county <- function(dataset) {
  county_list <- list()
  
  for(c_name in unique(dataset$COUNTY)) {
    
    if(c_name == "Pending" || c_name == "Out of State") {
      next
    }
    
    c_data <- dataset[dataset$COUNTY == c_name,]
    f_code <- fips[fips$county == c_name,1]
    if(length(f_code) == 0) {
      f_code <- "0"
    }
    c_data$FIPS <- f_code
    c_data <- list(c_data)
    
    county_list <- append(county_list, c_data)
  }
  
  return(county_list)
}

county_name_path <- function(path, county_name) {
  p <- paste(path, "/", county_name[1], ".csv", sep = "")
  return(p)
}

first_nonzero_index <- function(col) {
  
  if(unique(col)[1] == 0 && length(unique(col)) == 1) {
    return(1)
  }
  
  i <- 1
  i_found <- FALSE
  
  while(i_found == FALSE) {
    i <- i + 1
    
    if (col[i + 1] <= 10 && col[i + 1] > 0 && col[i] == 0) {
      return(i)
    } else if (col[i] > 0) {
      return(i)
    } else if (col[i] < 0) {
      return(i)
    }
  }
}

seven_day_average <- function(daily_cases) {
  mov_avg <- daily_cases
  
  for(i in 1:length(daily_cases)) {
    if(i < 7) {
      mov_avg[i] <- (sum(daily_cases[1:i]) / i)
    } else {
      mov_avg[i] <- (sum(daily_cases[(i-6):i]) / 7)
    }
  }
  
  return(mov_avg)
}

######################################
# Plots
######################################

total_cases_plot <- function(ds) {
  
  county <- paste(ds$COUNTY[1], 'County')
  g_title <- paste('Total Confirmed COVID-19 Cases in ', county, ", TN", sep = '')
  
  fdate <- format(ds$DATE, '%Y-%m-%d')
  
  #result <- plot_ly(data = ds) 
  #result <- result %>% add_trace(x = ~DATE, 
  #                               y = ~TOTAL_CASES, type = 'scatter', 
  #                               mode = 'lines',
  #                              line = list(color = 'rgba(0, 164, 179, 1)'))
  #result <- result %>% layout(xaxis = list(title = 'Date'),
  #                            yaxis = list(title = 'Confirmed Cases'),
  #                            title = g_title)
  #result <- result %>% config(displayModeBar = FALSE, displaylogo = FALSE, responsive = TRUE)
  
  export <- list(xval = fdate, 
                 yval = ds$TOTAL_CASES, 
                 linecolor = 'rgb(0, 164, 179)', 
                 gtitle = g_title, 
                 ytitle = "Confirmed Cases",
                 type = "total")
  
  return(export)
}

total_deaths_plot <- function(ds) {
  
  county <- paste(ds$COUNTY[1], 'County')
  g_title <- paste('Total Confirmed COVID-19 Deaths in ', county, ", TN", sep = '')
  
  fdate <- format(ds$DATE, '%Y-%m-%d')
  
  #result <- plot_ly(data = ds) 
  #result <- result %>% add_trace(x = ~DATE, 
  #                               y = ~TOTAL_DEATHS, type = 'scatter', 
  #                               mode = 'lines', line = list(color = 'rgba(255, 93, 87, 255'))
  #result <- result %>% layout(xaxis = list(title = 'Date'),
  #                            yaxis = list(title = 'Confirmed Deaths'),
  #                            title = g_title)
  #result <- result %>% config(displayModeBar = FALSE, displaylogo = FALSE, responsive = TRUE)
  
  export <- list(xval = fdate, 
                 yval = ds$TOTAL_DEATHS, 
                 linecolor = 'rgb(255, 93, 87)', 
                 gtitle = g_title, 
                 ytitle = "Confirmed Deaths",
                 type = "total")
  
  return(export)
}

daily_cases_plot <- function(ds) {
  mov_avg <- seven_day_average(ds$NEW_CASES)
  
  county <- paste(ds$COUNTY[1], 'County')
  g_title <- paste('Daily Confirmed COVID-19 Cases in ', county, ", TN", sep = '')
  
  fdate <- format(ds$DATE, '%Y-%m-%d')
  
  #result <- plot_ly(data = ds, showlegend = FALSE, hoverinfo = 'skip') 
  #result <- result %>% add_trace(x = ~DATE, 
  #                               y = ~NEW_CASES, type = 'bar', 
  #                               marker = list(color = 'rgba(0, 182, 199, 0.5)'),
  #                               hoverinfo = 'x+y')
  #result <- result %>% add_trace(x = ~DATE, 
  #                               y = mov_avg, 
  #                               type = 'scatter',
  #                               mode  = 'lines',
  #                               fill = 'tozeroy',
  #                               fillcolor = 'rgba(0, 182, 199, 0.25)', 
  #                               line = list(color = 'rgba(0, 164, 179, 1)'),
  #                               hoverinfo = 'skip')
  #result <- result %>% layout(xaxis = list(title = 'Date'),
  #                           yaxis = list(title = 'Daily Confirmed Cases'),
  #                            title = g_title)
  #result <- result %>% config(displayModeBar = FALSE, displaylogo = FALSE, responsive = TRUE)
  
  export <- list(xval = fdate, 
                 yval = ds$NEW_CASES, 
                 barcolor = 'rgba(0, 182, 199, 0.5)', 
                 fillcolor = 'rgba(0, 182, 199, 0.25)', 
                 movingAverage = mov_avg,
                 movingLineColor = 'rgb(0, 164, 179)',
                 gtitle = g_title, 
                 ytitle = "Daily Confirmed Cases",
                 type = "daily")
  
  return(export)
}

daily_deaths_plot <- function(ds) {
  mov_avg <- seven_day_average(ds$NEW_DEATHS)
  
  county <- paste(ds$COUNTY[1], 'County')
  g_title <- paste('Daily Confirmed COVID-19 Deaths in ', county, ", TN", sep = '')
  
  fdate <- format(ds$DATE, '%Y-%m-%d')
  
  #result <- plot_ly(data = ds, showlegend = FALSE, hoverinfo = 'skip') 
  #result <- result %>% add_trace(x = ~DATE, 
  #                               y = ~NEW_DEATHS, type = 'bar', 
  #                               marker = list(color = 'rgba(255, 124, 120, 0.5)'),
  #                               hoverinfo = 'x+y')
  #result <- result %>% add_trace(x = ~DATE, 
  #                               y = mov_avg, 
  #                               type = 'scatter',
  #                               mode  = 'lines',
  #                               fill = 'tozeroy',
  #                              fillcolor = 'rgba(255, 124, 120, 0.25)', 
  #                               line = list(color = 'rgba(255, 93, 87, 1)'),
  #                               hoverinfo = 'skip')
  #result <- result %>% layout(xaxis = list(title = 'Date'),
  #                            yaxis = list(title = 'Daily Confirmed Deaths'),
  #                            title = g_title)
  #result <- result %>% config(displayModeBar = FALSE, displaylogo = FALSE, responsive = TRUE)
  
  export <- list(xval = fdate, 
                 yval = ds$NEW_DEATHS, 
                 barcolor = 'rgba(255, 124, 120, 0.5)', 
                 fillcolor = 'rgba(255, 124, 120, 0.25)', 
                 movingAverage = mov_avg,
                 movingLineColor = 'rgb(255, 93, 87)',
                 gtitle = g_title, 
                 ytitle = "Daily Confirmed Deaths",
                 type = "daily")
  
  return(export)
}

daily_testing_data_plot <- function(ds) {
  
  county <- paste(ds$COUNTY[1], 'County')
  g_title <- paste('Daily Testing Data for ', county, ", TN", sep = '')
  
  start_index <- first_nonzero_index(ds$NEW_TESTS)
  mod_date_range <- ds$DATE[start_index:length(ds$DATE)]
  mod_test_range <- ds$NEW_TESTS[start_index:length(ds$NEW_TESTS)]
  mod_pos_range <- ds$NEW_CASES[start_index:length(ds$NEW_CASES)]
  
  y2 <- list(
    overlaying = "y",
    side = "right",
    title = "Positive (%)",
    rangemode = 'tozero',
    showgrid = FALSE
  )
  
  percent_positve <- (mod_pos_range / mod_test_range) * 100
  percent_positve <- round(percent_positve, 1)
  percent_positve[which(!is.finite(percent_positve))] <- 0.0
  
  result <- plot_ly(data = ds, showlegend = TRUE, hoverinfo = 'skip') 
  result <- result %>% add_trace(x = mod_date_range, 
                                 y = mod_test_range, 
                                 type = 'bar',
                                 name = 'Total Tests',
                                 hoverinfo = 'x+y',
                                 color = I('rgba(206, 162, 219, 1)'))
  result <- result %>% add_trace(x = mod_date_range, 
                                 y = mod_pos_range, 
                                 type = 'bar', 
                                 name = 'Positive Tests',
                                 hoverinfo = 'x+y',
                                 color = I('rgba(0, 182, 199, 1)'))
  result <- result %>% add_trace(x = mod_date_range,
                                 y = percent_positve,
                                 type = 'scatter',
                                 mode = 'lines',
                                 name = 'Positive (%)',
                                 yaxis = "y2",
                                 hoverinfo = 'x+y',
                                 color = I('rgba(191, 23, 23, 1)'))
  result <- result %>% layout(xaxis = list(title = 'Date'),
                              yaxis = list(title = 'Daily Tests'),
                              yaxis2 = y2,
                              barmode = 'overlay',
                              title = g_title,
                              legend = list(y = -.3, orientation = 'h'),
                              margin = list(r = 45))
  result <- result %>% config(displayModeBar = FALSE, displaylogo = FALSE, responsive = TRUE)
  
  return(result)
}

current_active_cases_plot <- function(ds) {
  
  county <- paste(ds$COUNTY[1], 'County')
  g_title <- paste('Active COVID-19 Cases in ', county, ", TN", sep = '')
  
  start_index <- first_nonzero_index(ds$TOTAL_ACTIVE)
  mod_active <- ds$TOTAL_ACTIVE[start_index:length(ds$TOTAL_ACTIVE)]
  mod_date <- ds$DATE[start_index:length(ds$DATE)]
  
  fdate <- format(mod_date, '%Y-%m-%d')
  
  #result <- plot_ly(data = ds) 
  #result <- result %>% add_trace(x = mod_date, 
  #                               y = mod_active, type = 'scatter', 
  #                               mode = 'lines',
  #                               line = list(color = 'rgba(0, 219, 69, 255'))
  #result <- result %>% layout(xaxis = list(title = 'Date'),
  #                            yaxis = list(title = 'Active Cases'),
  #                            title = g_title)
  #result <- result %>% config(displayModeBar = FALSE, displaylogo = FALSE, responsive = TRUE)
  
  export <- list(xval = fdate, 
                 yval = mod_active, 
                 linecolor = 'rgb(0, 219, 69)', 
                 gtitle = g_title, 
                 ytitle = "Active Cases",
                 type = "total")
  
  return(export)
}

daily_active_cases_plot <- function(ds) {
  start_index <- first_nonzero_index(ds$NEW_ACTIVE)
  mod_new_active <- ds$NEW_ACTIVE[start_index:length(ds$NEW_ACTIVE)]
  mod_date <- ds$DATE[start_index:length(ds$DATE)]
  
  mov_avg <- seven_day_average(mod_new_active)
  
  county <- paste(ds$COUNTY[1], 'County')
  g_title <- paste('Daily Change of Active COVID-19 Cases in ', county, ", TN", sep = '')
  
  fdate <- format(mod_date, '%Y-%m-%d')

  #result <- plot_ly(data = ds, showlegend = FALSE, hoverinfo = 'skip') 
  #result <- result %>% add_trace(x = mod_date, 
  #                               y = mod_new_active, type = 'bar', 
  #                               marker = list(color = 'rgba(56, 209, 105 0.5)'),
  #                               hoverinfo = 'x+y')
  #result <- result %>% add_trace(x = mod_date, 
  #                               y = mov_avg, 
  #                              type = 'scatter',
  #                               mode  = 'lines',
  #                               fill = 'tozeroy',
  #                               fillcolor = 'rgba(56, 209, 105 0.25)', 
  #                               line = list(color = 'rgba(0, 219, 69, 255'),
  #                               hoverinfo = 'skip')
  #result <- result %>% layout(xaxis = list(title = 'Date'),
  #                            yaxis = list(title = 'Daily Change of Active Cases'),
  #                            title = g_title)
  #result <- result %>% config(displayModeBar = FALSE, displaylogo = FALSE, responsive = TRUE)
  
  export <- list(xval = fdate, 
                 yval = mod_new_active, 
                 barcolor = 'rgba(56, 209, 105, 0.5)', 
                 fillcolor = 'rgba(56, 209, 105, 0.25)', 
                 movingAverage = mov_avg,
                 movingLineColor = 'rgb(0, 219, 69)',
                 gtitle = g_title, 
                 ytitle = "Daily Change of Active Cases",
                 type = "daily")
  
  return(export)
}

total_recovered_plot <- function(ds) {
  
  county <- paste(ds$COUNTY[1], 'County')
  g_title <- paste('Total Recovered COVID-19 Cases in ', county, ", TN", sep = '')
  
  start_index <- first_nonzero_index(ds$TOTAL_RECOVERED)
  mod_rec <- ds$TOTAL_RECOVERED[start_index:length(ds$TOTAL_RECOVERED)]
  mod_date <- ds$DATE[start_index:length(ds$DATE)]
  
  fdate <- format(mod_date, '%Y-%m-%d')
  
  #result <- plot_ly(data = ds) 
  #result <- result %>% add_trace(x = mod_date, 
  #                               y = mod_rec, type = 'scatter', 
  #                               mode = 'lines',
  #                               line = list(color = 'rgba(231, 235, 30, 1)'))
  #result <- result %>% layout(xaxis = list(title = 'Date'),
  #                            yaxis = list(title = 'Recovered Cases'),
  #                            title = g_title)
  #result <- result %>% config(displayModeBar = FALSE, displaylogo = FALSE, responsive = TRUE)
  
  export <- list(xval = fdate, 
                 yval = mod_rec, 
                 linecolor = 'rgb(231, 235, 30)', 
                 gtitle = g_title, 
                 ytitle = "Recovered Cases",
                 type = "total")
  
  return(export)
}

daily_recovered_plot <- function(ds) {
  start_index <- first_nonzero_index(ds$NEW_RECOVERED)
  mod_daily_rec <- ds$NEW_RECOVERED[start_index:length(ds$NEW_RECOVERED)]
  mod_date <- ds$DATE[start_index:length(ds$DATE)]
  
  mov_avg <- seven_day_average(mod_daily_rec)
  
  county <- paste(ds$COUNTY[1], 'County')
  g_title <- paste('Daily Recovered COVID-19 Cases in ', county, ", TN", sep = '')
  
  fdate <- format(mod_date, '%Y-%m-%d')
  
  #result <- plot_ly(data = ds, showlegend = FALSE, hoverinfo = 'skip') 
  #result <- result %>% add_trace(x = mod_date, 
  #                               y = mod_daily_rec, type = 'bar', 
  #                               marker = list(color = 'rgba(197, 201, 22, 0.5)'),
  #                               hoverinfo = 'x+y')
  #result <- result %>% add_trace(x = mod_date, 
  #                               y = mov_avg, 
  #                               type = 'scatter',
  #                               mode  = 'lines',
  #                               fill = 'tozeroy',
  #                               fillcolor = 'rgba(197, 201, 22, 0.25)', 
  #                               line = list(color = 'rgba(231, 235, 30, 1)'),
  #                               hoverinfo = 'skip')
  #result <- result %>% layout(xaxis = list(title = 'Date'),
  #                           yaxis = list(title = 'Daily Recovered Cases'),
  #                            title = g_title)
  #result <- result %>% config(displayModeBar = FALSE, displaylogo = FALSE, responsive = TRUE)
  
  export <- list(xval = fdate, 
                 yval = mod_daily_rec, 
                 barcolor = 'rgba(197, 201, 22, 0.5)', 
                 fillcolor = 'rgba(197, 201, 22, 0.25)', 
                 movingAverage = mov_avg,
                 movingLineColor = 'rgb(231, 235, 30)',
                 gtitle = g_title, 
                 ytitle = "Daily Recovered Cases",
                 type = "daily")
  
  return(export)
}

total_hospitalized_plot <- function(ds) {
  
  county <- paste(ds$COUNTY[1], 'County')
  g_title <- paste('Total COVID-19 Hospitalizations in ', county, ", TN", sep = '')
  
  start_index <- first_nonzero_index(ds$TOTAL_HOSPITALIZED)
  mod_hosp <- ds$TOTAL_HOSPITALIZED[start_index:length(ds$TOTAL_HOSPITALIZED)]
  mod_date <- ds$DATE[start_index:length(ds$DATE)]
  
  fdate <- format(mod_date, '%Y-%m-%d')
  
  #result <- plot_ly(data = ds) 
  #result <- result %>% add_trace(x = mod_date, 
  #                               y = mod_hosp, type = 'scatter', 
  #                               mode = 'lines',
  #                               line = list(color = 'rgba(245, 99, 15, 1)'))
  #result <- result %>% layout(xaxis = list(title = 'Date'),
  #                            yaxis = list(title = 'Hospitalized Cases'),
  #                            title = g_title)
  #result <- result %>% config(displayModeBar = FALSE, displaylogo = FALSE, responsive = TRUE)
  
  export <- list(xval = fdate, 
                 yval = mod_hosp, 
                 linecolor = 'rgb(245, 99, 15)', 
                 gtitle = g_title, 
                 ytitle = "Hospitalized Cases",
                 type = "total")
  
  return(export)
}

daily_hospitalizations_plot <- function(ds) {
  start_index <- first_nonzero_index(ds$NEW_HOSPITALIZED)
  mod_daily_hosp <- ds$NEW_HOSPITALIZED[start_index:length(ds$NEW_HOSPITALIZED)]
  mod_date <- ds$DATE[start_index:length(ds$DATE)]
  
  mov_avg <- seven_day_average(mod_daily_hosp)
  
  county <- paste(ds$COUNTY[1], 'County')
  g_title <- paste('Daily COVID-19 Hospitalizations in ', county, ", TN", sep = '')
  
  fdate <- format(mod_date, '%Y-%m-%d')
  
  #result <- plot_ly(data = ds, showlegend = FALSE, hoverinfo = 'skip') 
  #result <- result %>% add_trace(x = mod_date, 
  #                               y = mod_daily_hosp, type = 'bar', 
  #                               marker = list(color = 'rgba(245, 136, 73, 0.5)'),
  #                               hoverinfo = 'x+y')
  #result <- result %>% add_trace(x = mod_date, 
  #                               y = mov_avg, 
  #                               type = 'scatter',
  #                               mode  = 'lines',
  #                               fill = 'tozeroy',
  #                               fillcolor = 'rgba(245, 136, 73, 0.25)', 
  #                               line = list(color = 'rgba(245, 99, 15, 1)'),
  #                               hoverinfo = 'skip')
  #result <- result %>% layout(xaxis = list(title = 'Date'),
  #                            yaxis = list(title = 'Daily Hospitalizations'),
  #                            title = g_title)
  #result <- result %>% config(displayModeBar = FALSE, displaylogo = FALSE, responsive = TRUE)
  
  export <- list(xval = fdate, 
                 yval = mod_daily_hosp, 
                 barcolor = 'rgba(245, 136, 73, 0.5)', 
                 fillcolor = 'rgba(245, 136, 73, 0.25)', 
                 movingAverage = mov_avg,
                 movingLineColor = 'rgb(245, 99, 15)',
                 gtitle = g_title, 
                 ytitle = "Daily Hospitalizations",
                 type = "daily")
  
  return(export)
}

######################################
# Maps
######################################

total_cases_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(fips = c(NA), total_cases = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    fips <- cty$FIPS[1]
    total_cases <- cty$TOTAL_CASES[length(cty$TOTAL_CASES)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(fips, total_cases)
    } else {
      df[nrow(df) + 1,] <- c(fips, total_cases)
    }
    
  }
  
  df$total_cases <- as.numeric(df$total_cases)
  
  fig <- fig %>% add_trace(
    type="choroplethmapbox",
    geojson=county_geojson,
    locations=df$fips,
    z=df$total_cases,
    featureidkey="properties.FIPS",
    colorscale="Viridis",
    marker=list(line=list(
      width=0),
      opacity=0.5
    )
  )
  
  fig <- fig %>% layout(
    mapbox=list(
      style="carto-positron",
      zoom=4.75,
      center=list(lat=35.51, lon=-86))
  )
  
  return(fig)
}

total_deaths_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(fips = c(NA), total_deaths = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    fips <- cty$FIPS[1]
    total_deaths <- cty$TOTAL_DEATHS[length(cty$TOTAL_DEATHS)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(fips, total_deaths)
    } else {
      df[nrow(df) + 1,] <- c(fips, total_deaths)
    }
    
  }
  
  df$total_deaths <- as.numeric(df$total_deaths)
  
  fig <- fig %>% add_trace(
    type="choroplethmapbox",
    geojson=county_geojson,
    locations=df$fips,
    z=df$total_deaths,
    featureidkey="properties.FIPS",
    colorscale="Viridis",
    marker=list(line=list(
      width=0),
      opacity=0.5
    )
  )
  
  fig <- fig %>% layout(
    mapbox=list(
      style="carto-positron",
      zoom=4.75,
      center=list(lat=35.51, lon=-86))
  )
  
  return(fig)
}

daily_cases_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(fips = c(NA), new_cases = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    fips <- cty$FIPS[1]
    new_cases <- cty$NEW_CASES[length(cty$NEW_CASES)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(fips, new_cases)
    } else {
      df[nrow(df) + 1,] <- c(fips, new_cases)
    }

  }
  
  df$new_cases <- as.numeric(df$new_cases)
  
  fig <- fig %>% add_trace(
    type="choroplethmapbox",
    geojson=county_geojson,
    locations=df$fips,
    z=df$new_cases,
    featureidkey="properties.FIPS",
    colorscale="Viridis",
    marker=list(line=list(
      width=0),
      opacity=0.5
    )
  )
  
  fig <- fig %>% layout(
    mapbox=list(
      style="carto-positron",
      zoom=4.75,
      center=list(lat=35.51, lon=-86))
  )
  
  return(fig)
}

daily_deaths_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(fips = c(NA), new_deaths = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    fips <- cty$FIPS[1]
    new_deaths <- cty$NEW_DEATHS[length(cty$NEW_DEATHS)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(fips, new_deaths)
    } else {
      df[nrow(df) + 1,] <- c(fips, new_deaths)
    }
    
  }
  
  df$new_deaths <- as.numeric(df$new_deaths)
  
  fig <- fig %>% add_trace(
    type="choroplethmapbox",
    geojson=county_geojson,
    locations=df$fips,
    z=df$new_deaths,
    featureidkey="properties.FIPS",
    colorscale="Viridis",
    marker=list(line=list(
      width=0),
      opacity=0.5
    )
  )
  
  fig <- fig %>% layout(
    mapbox=list(
      style="carto-positron",
      zoom=4.75,
      center=list(lat=35.51, lon=-86))
  )
  
  return(fig)
}

total_tests_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(fips = c(NA), total_tests = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    fips <- cty$FIPS[1]
    total_tests <- cty$TOTAL_TESTS[length(cty$TOTAL_TESTS)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(fips, total_tests)
    } else {
      df[nrow(df) + 1,] <- c(fips, total_tests)
    }
    
  }
  
  df$total_tests <- as.numeric(df$total_tests)
  
  fig <- fig %>% add_trace(
    type="choroplethmapbox",
    geojson=county_geojson,
    locations=df$fips,
    z=df$total_tests,
    featureidkey="properties.FIPS",
    colorscale="Viridis",
    marker=list(line=list(
      width=0),
      opacity=0.5
    )
  )
  
  fig <- fig %>% layout(
    mapbox=list(
      style="carto-positron",
      zoom=4.75,
      center=list(lat=35.51, lon=-86))
  )
  
  return(fig)
}

current_active_cases_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(fips = c(NA), current_active_cases = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    fips <- cty$FIPS[1]
    current_active_cases <- cty$TOTAL_ACTIVE[length(cty$TOTAL_ACTIVE)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(fips, current_active_cases)
    } else {
      df[nrow(df) + 1,] <- c(fips, current_active_cases)
    }
    
  }
  
  df$current_active_cases <- as.numeric(df$current_active_cases)
  
  fig <- fig %>% add_trace(
    type="choroplethmapbox",
    geojson=county_geojson,
    locations=df$fips,
    z=df$current_active_cases,
    featureidkey="properties.FIPS",
    colorscale="Viridis",
    marker=list(line=list(
      width=0),
      opacity=0.5
    )
  )
  
  fig <- fig %>% layout(
    mapbox=list(
      style="carto-positron",
      zoom=4.75,
      center=list(lat=35.51, lon=-86))
  )
  
  return(fig)
}

daily_active_cases_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(fips = c(NA), daily_active_cases = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    fips <- cty$FIPS[1]
    daily_active_cases <- cty$NEW_ACTIVE[length(cty$NEW_ACTIVE)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(fips, daily_active_cases)
    } else {
      df[nrow(df) + 1,] <- c(fips, daily_active_cases)
    }
    
  }
  
  df$daily_active_cases <- as.numeric(df$daily_active_cases)
  
  fig <- fig %>% add_trace(
    type="choroplethmapbox",
    geojson=county_geojson,
    locations=df$fips,
    z=df$daily_active_cases,
    featureidkey="properties.FIPS",
    colorscale="Viridis",
    marker=list(line=list(
      width=0),
      opacity=0.5
    )
  )
  
  fig <- fig %>% layout(
    mapbox=list(
      style="carto-positron",
      zoom=4.75,
      center=list(lat=35.51, lon=-86))
  )
  
  return(fig)
}

total_recovered_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(fips = c(NA), total_recovered = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    fips <- cty$FIPS[1]
    total_recovered <- cty$TOTAL_RECOVERED[length(cty$TOTAL_RECOVERED)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(fips, total_recovered)
    } else {
      df[nrow(df) + 1,] <- c(fips, total_recovered)
    }
    
  }
  
  df$total_recovered <- as.numeric(df$total_recovered)
  
  fig <- fig %>% add_trace(
    type="choroplethmapbox",
    geojson=county_geojson,
    locations=df$fips,
    z=df$total_recovered,
    featureidkey="properties.FIPS",
    colorscale="Viridis",
    marker=list(line=list(
      width=0),
      opacity=0.5
    )
  )
  
  fig <- fig %>% layout(
    mapbox=list(
      style="carto-positron",
      zoom=4.75,
      center=list(lat=35.51, lon=-86))
  )
  
  return(fig)
}

daily_recovered_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(fips = c(NA), daily_recovered = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    fips <- cty$FIPS[1]
    daily_recovered <- cty$NEW_RECOVERED[length(cty$NEW_RECOVERED)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(fips, daily_recovered)
    } else {
      df[nrow(df) + 1,] <- c(fips, daily_recovered)
    }
    
  }
  
  df$daily_recovered <- as.numeric(df$daily_recovered)
  
  fig <- fig %>% add_trace(
    type="choroplethmapbox",
    geojson=county_geojson,
    locations=df$fips,
    z=df$daily_recovered,
    featureidkey="properties.FIPS",
    colorscale="Viridis",
    marker=list(line=list(
      width=0),
      opacity=0.5
    )
  )
  
  fig <- fig %>% layout(
    mapbox=list(
      style="carto-positron",
      zoom=4.75,
      center=list(lat=35.51, lon=-86))
  )
  
  return(fig)
}

total_hospitalizations_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(fips = c(NA), total_hospitalizations = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    fips <- cty$FIPS[1]
    total_hospitalizations <- cty$TOTAL_HOSPITALIZED[length(cty$TOTAL_HOSPITALIZED)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(fips, total_hospitalizations)
    } else {
      df[nrow(df) + 1,] <- c(fips, total_hospitalizations)
    }
    
  }
  
  df$total_hospitalizations <- as.numeric(df$total_hospitalizations)
  
  fig <- fig %>% add_trace(
    type="choroplethmapbox",
    geojson=county_geojson,
    locations=df$fips,
    z=df$total_hospitalizations,
    featureidkey="properties.FIPS",
    colorscale="Viridis",
    marker=list(line=list(
      width=0),
      opacity=0.5
    )
  )
  
  fig <- fig %>% layout(
    mapbox=list(
      style="carto-positron",
      zoom=4.75,
      center=list(lat=35.51, lon=-86))
  )
  
  return(fig)
}

daily_hospitalizations_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(fips = c(NA), new_hospitalizations = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    fips <- cty$FIPS[1]
    new_hospitalizations <- cty$NEW_HOSPITALIZED[length(cty$NEW_HOSPITALIZED)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(fips, new_hospitalizations)
    } else {
      df[nrow(df) + 1,] <- c(fips, new_hospitalizations)
    }
    
  }
  
  df$new_hospitalizations <- as.numeric(df$new_hospitalizations)
  
  fig <- fig %>% add_trace(
    type="choroplethmapbox",
    geojson=county_geojson,
    locations=df$fips,
    z=df$new_hospitalizations,
    featureidkey="properties.FIPS",
    colorscale="Viridis",
    marker=list(line=list(
      width=0),
      opacity=0.5
    )
  )
  
  fig <- fig %>% layout(
    mapbox=list(
      style="carto-positron",
      zoom=4.75,
      center=list(lat=35.51, lon=-86))
  )
  
  return(fig)
}


######################################
# Script
######################################

print("Creating directories...")
del_temp()
if(!dir.exists("datasets")) {
  dir.create("datasets")
}
if(!dir.exists("counties")) {
  dir.create("counties")
}
if(!dir.exists("plots")) {
  dir.create("plots")
}
if(!dir.exists("maps")) {
  dir.create("maps")
}
if(!dir.exists("alljson")) {
  dir.create("alljson")
}
dir.create("temp")

age_dataset_url <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-Age.XLSX"
county_new_dataset_url <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-County-New.XLSX"
daily_case_info_dataset_url <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-Daily-Case-Info.XLSX"
race_ethnicity_sex_dataset_url <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-RaceEthSex.XLSX"

age_dataset_filename_prefix <- "age_dataset"
county_new_dataset_filename_prefix <- "county_new_dataset"
daily_case_info_dataset_filename_prefix <- "daily_case_info_dataset"
race_ethnicity_sex_dataset_filename_prefix <- "race_ethnicity_sex_dataset_url"

age_temp_path <- temp_path(add_xlsx(age_dataset_filename_prefix))
county_new_temp_path <- temp_path(add_xlsx(county_new_dataset_filename_prefix))
daily_case_info_temp_path <- temp_path(add_xlsx(daily_case_info_dataset_filename_prefix))
race_ethnicity_sex_temp_path <- temp_path(add_xlsx(race_ethnicity_sex_dataset_filename_prefix))

print("Downloading files...")
download.file(url = age_dataset_url, destfile = age_temp_path)
download.file(url = county_new_dataset_url, destfile = county_new_temp_path)
download.file(url = daily_case_info_dataset_url, destfile = daily_case_info_temp_path)
download.file(url = race_ethnicity_sex_dataset_url, destfile = race_ethnicity_sex_temp_path)

print("Loading files...")

age_col_types <- c("date", "text", "numeric", "numeric", "numeric", 
                   "numeric", "numeric", "numeric")

county_new_col_types <- c("date", "text", "numeric", "numeric","numeric",
           "numeric","numeric","numeric","numeric","numeric",
           "numeric","numeric","numeric","numeric","numeric",
           "numeric","numeric", "numeric")

daily_case_info_col_types <- c("date", "numeric", "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric", "numeric")

race_ethnicity_sex_col_types <- c("date", "text", "text", "numeric", "numeric", "numeric", "numeric")

fips_col_types <- c("character", "character")

age_dataset <- load_file(age_temp_path, age_col_types)
county_new_dataset <- load_file(county_new_temp_path, county_new_col_types)
daily_case_info_dataset <- load_file(daily_case_info_temp_path, daily_case_info_col_types)
race_ethnicity_sex_dataset <- load_file(race_ethnicity_sex_temp_path, race_ethnicity_sex_col_types)
fips <- read.csv(file = 'fips/tn-counties.csv', stringsAsFactors = FALSE, colClasses = fips_col_types)
county_geojson <- rjson::fromJSON(file = "geojson/tn_counties.json")

datasets <- list(age_dataset,
                 county_new_dataset,
                 daily_case_info_dataset,
                 race_ethnicity_sex_dataset)

if(!all_same_date(datasets)) {
  stop("Datasets are not from the same day")
}

curr_date <- curr_dataset_date(datasets)
fcurr_date <- formatted_date(curr_date)

if(is.element(curr_date, dataset_dir())) {
  msg <- paste("INFO: Datasets already exist for", fcurr_date, "in 'datasets' folder.")
  print(msg)
  print("Deleting temporary files...")
  del_temp()
} else {
  print(paste("Dataset files do not exist. Creating dataset files for ", fcurr_date, " ...", sep = ""))
  dpath <- dataset_path(curr_date)
  dir.create(dpath)
  
  tfiles <- list.files("temp")
  
  ffiles <- lapply(tfiles, function(x) {paste("temp", x, sep="/")})
  ffiles <- do.call(c, ffiles)
  
  file.copy(from = ffiles, to = dpath, overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
  print("Deleting temporary files...")
  del_temp()
}

print("Reformatting data...")
age_superlist <- split_by_date(age_dataset)
county_new_superlist <- split_by_county(county_new_dataset)
race_ethnicity_sex_superlist <- split_by_date(race_ethnicity_sex_dataset)

if(is.element(curr_date, counties_dir())) {
  msg <- paste("INFO: Cases information by county already exist for", fcurr_date, "in 'counties' folder.")
  print(msg)
} else {
  msg <- paste("Creating county .csv files for ", fcurr_date, "...", sep = "")
  print(msg)
  cpath <- counties_path(curr_date)
  dir.create(cpath) 
  
  for (county in county_new_superlist) {
    county %>% write.csv(county_name_path(cpath, county$COUNTY), row.names = FALSE)
  }
}

if(!is.element(curr_date, plots_dir()) && is.element(curr_date, maps_dir())) {
  msg <- paste("INFO: Plot JSON data already exist for", fcurr_date, "in 'plots' and 'maps' folders.")
  print(msg)
} else {
  msg <- paste("Creating plotly JSON files for ", fcurr_date, "...", sep = "")
  print(msg)
  ppath <- plot_path(curr_date)
  dir.create(ppath)
  mpath <- map_path(curr_date)
  dir.create(mpath)
  
  c_list <- list()
  c_name_list <- c()
  
  for (county in county_new_superlist) {
    c_name <- county$COUNTY[1]
    c_name_list <- c(c_name_list, c_name)
    
    l_names <- c("total_cases", "total_deaths",
                 "daily_cases", "daily_deaths", "active_cases",
                 "daily_active", "total_recoveries", "daily_recoveries",
                 "total_hospitalized", "daily_hospitalized")
      
    tcp <- total_cases_plot(county)
    tdp <- total_deaths_plot(county)
    
    dcp <- daily_cases_plot(county)
    ddp <- daily_deaths_plot(county)
    
    cac <- current_active_cases_plot(county)
    dac <- daily_active_cases_plot(county)
    
    tr <- total_recovered_plot(county)
    dr <- daily_recovered_plot(county)
    
    th <- total_hospitalized_plot(county)
    dh <- daily_hospitalizations_plot(county)
    
    plot_list <- list(tcp, tdp, dcp, ddp, cac, dac, tr, dr, th, dh)
    names(plot_list) <- l_names
    
    c_list <- append(c_list, list(plot_list))
    
    #dtest <- daily_testing_data_plot(county)
    
    #c_dir <- paste(ppath, c_name, sep="/")
    
    #dir.create(c_dir)
    
    #tcp_file <- 'total_cases_plot.json'
    #tdp_file <- 'total_deaths_plot.json'
    #dcp_file <- 'daily_cases_plot.json'
    #ddp_file <- 'daily_deaths_plot.json'
    #dtest_file <- 'daily_testing_data_plot.json'
    #cac_file <- 'current_active_cases_plot.json'
    #dac_file <- 'daily_active_cases_plot.json'
    #tr_file <- 'total_recovered_plot.json'
    #dr_file <- 'daily_recovered_plot.json'
    #th_file <- 'total_hospitalized_plot.json'
    #dh_file <- 'daily_hospitalizations_plot.json'
    
    #tcp <- plotly_json(tcp, FALSE)
    #tdp <- plotly_json(tdp, FALSE)
    #dcp <- plotly_json(dcp, FALSE)
    #ddp <- plotly_json(ddp, FALSE)
    #dtest <- plotly_json(dtest, FALSE)
    #cac <- plotly_json(cac, FALSE)
    #dac <- plotly_json(dac, FALSE)
    #tr <- plotly_json(tr, FALSE)
    #dr <- plotly_json(dr, FALSE)
    #th <- plotly_json(th, FALSE)
    #dh <- plotly_json(dh, FALSE)
    
    #write(tcp, paste(c_dir, tcp_file, sep = "/"))
    #write(tdp, paste(c_dir, tdp_file, sep = "/"))
    #write(dcp, paste(c_dir, dcp_file, sep = "/"))
    #write(ddp, paste(c_dir, ddp_file, sep = "/"))
    #write(dtest, paste(c_dir, dtest_file, sep = "/"))
    #write(cac, paste(c_dir, cac_file, sep = "/"))
    #write(dac, paste(c_dir, dac_file, sep = "/"))
    #write(tr, paste(c_dir, tr_file, sep = "/"))
    #write(dr, paste(c_dir, dr_file, sep = "/"))
    #write(th, paste(c_dir, th_file, sep = "/"))
    #write(dh, paste(c_dir, dh_file, sep = "/"))
  }
  
  names(c_list) <- c_name_list
  
  json_list <- toJSON(c_list)
  write(json_list, file = "counties.json")
  
  #tcm <- total_cases_map(county_new_superlist)
  #tdm <- total_deaths_map(county_new_superlist)
  
  #dcm <- daily_cases_map(county_new_superlist)
  #ddm <- daily_deaths_map(county_new_superlist)
  
  #dtestm <- total_tests_map(county_new_superlist)
  
  #cacm <- current_active_cases_map(county_new_superlist)
  #dacm <- daily_active_cases_map(county_new_superlist)
  
  #trm <- total_recovered_map(county_new_superlist)
  #drm <- daily_recovered_map(county_new_superlist)
  
  #thm <- total_hospitalizations_map(county_new_superlist)
  #dhm <- daily_hospitalizations_map(county_new_superlist)
  
  #tcm_file <- 'total_cases_map.json'
  #tdm_file <- 'total_deaths_map.json'
  #dcm_file <- 'daily_cases_map.json'
  #ddm_file <- 'daily_deaths_map.json'
  #dtestm_file <- 'daily_testing_data_map.json'
  #cacm_file <- 'current_active_cases_map.json'
  #dacm_file <- 'daily_active_cases_map.json'
  #trm_file <- 'total_recovered_map.json'
  #drm_file <- 'daily_recovered_map.json'
  #thm_file <- 'total_hospitalized_map.json'
  #dhm_file <- 'daily_hospitalizations_map.json'
  
  #tcm <- plotly_json(tcm, FALSE)
  #tdm <- plotly_json(tdm, FALSE)
  #dcm <- plotly_json(dcm, FALSE)
  #ddm <- plotly_json(ddm, FALSE)
  #dtestm <- plotly_json(dtestm, FALSE)
  #cacm <- plotly_json(cacm, FALSE)
  #dacm <- plotly_json(dacm, FALSE)
  #trm <- plotly_json(trm, FALSE)
  #drm <- plotly_json(drm, FALSE)
  #thm <- plotly_json(thm, FALSE)
  #dhm <- plotly_json(dhm, FALSE)
  
  #write(tcm, paste(mpath, tcm_file, sep = "/"))
  #write(tdm, paste(mpath, tdm_file, sep = "/"))
  #write(dcm, paste(mpath, dcm_file, sep = "/"))
  #write(ddm, paste(mpath, ddm_file, sep = "/"))
  #write(dtestm, paste(mpath, dtestm_file, sep = "/"))
  #write(cacm, paste(mpath, cacm_file, sep = "/"))
  #write(dacm, paste(mpath, dacm_file, sep = "/"))
  #write(trm, paste(mpath, trm_file, sep = "/"))
  #write(drm, paste(mpath, drm_file, sep = "/"))
  #write(thm, paste(mpath, thm_file, sep = "/"))
  #write(dhm, paste(mpath, dhm_file, sep = "/"))
  
 # plot_files <- list()
  
 # county_files <- list.files(ppath, all.files = FALSE, no.. = TRUE)
  
  
  #for(cfold in county_files) {
   # files <- list.files(paste(ppath, cfold, sep="/"), all.files = FALSE, no.. = TRUE)
  #  json_list <- lapply(as.list(files), function(x) {x <- fromJSON(file = paste(ppath, cfold, x, sep="/"))})
  #  listnames <- gsub(".json$", "", files)
  #  names(json_list) <- listnames
    
  #  plot_files <- append(plot_files, list(json_list))
  #}
  
#  county_files <- tolower(county_files)
#  names(plot_files) <- county_files
  
#  map_file_list <- list.files(mpath, all.files = FALSE, no.. = TRUE)
#  map_files <- list()
  
#  mjson_list <- lapply(as.list(map_file_list), function(x) {x <- fromJSON(file = paste(mpath, x, sep="/"))})
##  m_list_names <- gsub(".json$", "", map_file_list)
#  names(mjson_list) <- m_list_names
  
 # alljson <- list(mjson_list, plot_files)
#  names(alljson) <- c("map", "graph")
  
 # write(toJSON(alljson), file = "alljson/allplots.json")
}

print("Successfully completed.")
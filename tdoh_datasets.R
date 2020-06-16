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
  
  fdate <- format(mod_date_range, '%Y-%m-%d')
  
  percent_positve <- (mod_pos_range / mod_test_range) * 100
  percent_positve <- round(percent_positve, 1)
  percent_positve[which(!is.finite(percent_positve))] <- 0.0
  percent_positve[which(percent_positve < 0.0)] <- 0.0
  
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
  
  export <- list(xval = fdate, 
                 totalTestVal = mod_test_range,
                 positiveVals = mod_pos_range,
                 percentPositive = percent_positve,
                 gtitle = g_title,
                 type = "testing")
  
  return(export)
}

current_active_cases_plot <- function(ds) {
  
  county <- paste(ds$COUNTY[1], 'County')
  g_title <- paste('Active COVID-19 Cases in ', county, ", TN", sep = '')
  
  start_index <- first_nonzero_index(ds$TOTAL_ACTIVE)
  mod_active <- ds$TOTAL_ACTIVE[start_index:length(ds$TOTAL_ACTIVE)]
  mod_date <- ds$DATE[start_index:length(ds$DATE)]
  
  fdate <- format(mod_date, '%Y-%m-%d')
  
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
  
  df <- data.frame(names = c(NA), total_cases = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    names <- cty$COUNTY[1]
    total_cases <- cty$TOTAL_CASES[length(cty$TOTAL_CASES)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(names, total_cases)
    } else {
      df[nrow(df) + 1,] <- c(names, total_cases)
    }
    
  }
  
  df$total_cases <- as.numeric(df$total_cases)
  
  export <- list(counties = df$names, 
                 z = df$total_cases, 
                 type = "cmap", 
                 mtitle = "Total COVID-19 Cases",
                 hovtext = "Total cases:",
                 col1 = 'rgb(255, 255, 255)',
                 col2 = 'rgb(164, 225, 252)',
                 col3 = 'rgb(105, 201, 245)',
                 col4 = 'rgb(3, 175, 255)',
                 col5 = 'rgb(0, 56, 120)')
  
  return(export)
}

total_deaths_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(names = c(NA), total_deaths = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    names <- cty$COUNTY[1]
    total_deaths <- cty$TOTAL_DEATHS[length(cty$TOTAL_DEATHS)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(names, total_deaths)
    } else {
      df[nrow(df) + 1,] <- c(names, total_deaths)
    }
    
  }
  
  df$total_deaths <- as.numeric(df$total_deaths)
  
  export <- list(counties = df$names, 
                 z = df$total_deaths, 
                 type = "cmap", 
                 mtitle = "Total COVID-19 Deaths",
                 hovtext = "Total deaths:",
                 col1 = 'rgb(255, 255, 255)',
                 col2 = 'rgb(255, 179, 179)',
                 col3 = 'rgb(240, 101, 101)',
                 col4 = 'rgb(199, 28, 28)',
                 col5 = 'rgb(148, 0, 0)')
  
  return(export)
}

daily_cases_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(names = c(NA), new_cases = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    name <- cty$COUNTY[1]
    new_cases <- cty$NEW_CASES[length(cty$NEW_CASES)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(name, new_cases)
    } else {
      df[nrow(df) + 1,] <- c(name, new_cases)
    }
    
  }
  
  df$new_cases <- as.numeric(df$new_cases)
  
  export <- list(counties = df$names, 
                 z = df$new_cases, 
                 type = "cmap", 
                 mtitle = paste("Daily COVID-19 Cases:", mcurr_date),
                 hovtext = "New cases:",
                 col1 = 'rgb(255, 255, 255)',
                 col2 = 'rgb(164, 225, 252)',
                 col3 = 'rgb(105, 201, 245)',
                 col4 = 'rgb(3, 175, 255)',
                 col5 = 'rgb(0, 56, 120)')
  
  return(export)
}

daily_deaths_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(names = c(NA), new_deaths = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    names <- cty$COUNTY[1]
    new_deaths <- cty$NEW_DEATHS[length(cty$NEW_DEATHS)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(names, new_deaths)
    } else {
      df[nrow(df) + 1,] <- c(names, new_deaths)
    }
    
  }
  
  df$new_deaths <- as.numeric(df$new_deaths)
  
  export <- list(counties = df$names, 
                 z = df$new_deaths, 
                 type = "cmap", 
                 mtitle = paste("Daily COVID-19 Deaths:", mcurr_date),
                 hovtext = "New deaths:",
                 col1 = 'rgb(255, 255, 255)',
                 col2 = 'rgb(252, 165, 164)',
                 col3 = 'rgb(240, 101, 101)',
                 col4 = 'rgb(199, 28, 28)',
                 col5 = 'rgb(148, 0, 0)')
  
  return(export)
}

total_tests_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(names = c(NA), total_tests = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    names <- cty$COUNTY[1]
    total_tests <- cty$TOTAL_TESTS[length(cty$TOTAL_TESTS)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(names, total_tests)
    } else {
      df[nrow(df) + 1,] <- c(names, total_tests)
    }
    
  }
  
  df$total_tests <- as.numeric(df$total_tests)
  
  export <- list(counties = df$names, 
                 z = df$total_tests, 
                 type = "cmap", 
                 mtitle = "Total COVID-19 Tests",
                 hovtext = "Total tests performed:",
                 col1 = 'rgb(255, 255, 255)',
                 col2 = 'rgb(232, 171, 255)',
                 col3 = 'rgb(219, 122, 255)',
                 col4 = 'rgb(160, 50, 201)',
                 col5 = 'rgb(114, 2, 156)')
  
  return(export)
}

current_active_cases_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(names = c(NA), current_active_cases = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    names <- cty$COUNTY[1]
    current_active_cases <- cty$TOTAL_ACTIVE[length(cty$TOTAL_ACTIVE)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(names, current_active_cases)
    } else {
      df[nrow(df) + 1,] <- c(names, current_active_cases)
    }
    
  }
  
  df$current_active_cases <- as.numeric(df$current_active_cases)
  
  export <- list(counties = df$names, 
                 z = df$current_active_cases, 
                 type = "cmap", 
                 mtitle = "Active COVID-19 Cases",
                 hovtext = "Active cases:",
                 col1 = 'rgb(255, 255, 255)',
                 col2 = 'rgb(158, 255, 182)',
                 col3 = 'rgb(105, 255, 142)',
                 col4 = 'rgb(50, 201, 88)',
                 col5 = 'rgb(0, 138, 34)')

  return(export)
}

daily_active_cases_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(names = c(NA), daily_active_cases = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    names <- cty$COUNTY[1]
    daily_active_cases <- cty$NEW_ACTIVE[length(cty$NEW_ACTIVE)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(names, daily_active_cases)
    } else {
      df[nrow(df) + 1,] <- c(names, daily_active_cases)
    }
    
  }
  
  df$daily_active_cases <- as.numeric(df$daily_active_cases)
  
  export <- list(counties = df$names, 
                 z = df$daily_active_cases, 
                 type = "cmap", 
                 mtitle = paste("Daily Change of Active COVID-19 Cases:", mcurr_date),
                 hovtext = "Net change of active cases:",
                 col1 = 'rgb(255, 255, 255)',
                 col2 = 'rgb(158, 255, 182)',
                 col3 = 'rgb(105, 255, 142)',
                 col4 = 'rgb(50, 201, 88)',
                 col5 = 'rgb(0, 138, 34)')
  
  return(export)
}

total_recovered_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(names = c(NA), total_recovered = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    names <- cty$COUNTY[1]
    total_recovered <- cty$TOTAL_RECOVERED[length(cty$TOTAL_RECOVERED)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(names, total_recovered)
    } else {
      df[nrow(df) + 1,] <- c(names, total_recovered)
    }
    
  }
  
  df$total_recovered <- as.numeric(df$total_recovered)
  
  export <- list(counties = df$names, 
                 z = df$total_recovered, 
                 type = "cmap", 
                 mtitle = "Total COVID-19 Recoveries",
                 hovtext = "Recovered cases:",
                 col1 = 'rgb(255, 255, 255)',
                 col2 = 'rgb(250, 255, 156)',
                 col3 = 'rgb(248, 255, 102)',
                 col4 = 'rgb(233, 242, 44)',
                 col5 = 'rgb(165, 173, 0)')
  
  return(export)
}

daily_recovered_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(names = c(NA), daily_recovered = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    names <- cty$COUNTY[1]
    daily_recovered <- cty$NEW_RECOVERED[length(cty$NEW_RECOVERED)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(names, daily_recovered)
    } else {
      df[nrow(df) + 1,] <- c(names, daily_recovered)
    }
    
  }
  
  df$daily_recovered <- as.numeric(df$daily_recovered)
  
  export <- list(counties = df$names, 
                 z = df$daily_recovered, 
                 type = "cmap", 
                 mtitle = paste("Daily COVID-19 Recoveries:", mcurr_date),
                 hovtext = "New recoveries:",
                 col1 = 'rgb(255, 255, 255)',
                 col2 = 'rgb(250, 255, 156)',
                 col3 = 'rgb(248, 255, 102)',
                 col4 = 'rgb(233, 242, 44)',
                 col5 = 'rgb(165, 173, 0)')
  
  return(export)
}

total_hospitalizations_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(names = c(NA), total_hospitalizations = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    names <- cty$COUNTY[1]
    total_hospitalizations <- cty$TOTAL_HOSPITALIZED[length(cty$TOTAL_HOSPITALIZED)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(names, total_hospitalizations)
    } else {
      df[nrow(df) + 1,] <- c(names, total_hospitalizations)
    }
    
  }
  
  df$total_hospitalizations <- as.numeric(df$total_hospitalizations)
  
  export <- list(counties = df$names, 
                 z = df$total_hospitalizations, 
                 type = "cmap", 
                 mtitle = "Total COVID-19 Hospitalizations",
                 hovtext = "Total hospitalizations:",
                 col1 = 'rgb(255, 255, 255)',
                 col2 = 'rgb(255, 204, 163)',
                 col3 = 'rgb(255, 175, 110)',
                 col4 = 'rgb(222, 134, 62)',
                 col5 = 'rgb(191, 86, 0)')
  
  return(export)
}

daily_hospitalizations_map <- function(superlist) {
  
  fig <- plot_ly()
  
  df <- data.frame(names = c(NA), new_hospitalizations = c(NA), stringsAsFactors = FALSE)
  
  for(cty in superlist) {
    names <- cty$COUNTY[1]
    new_hospitalizations <- cty$NEW_HOSPITALIZED[length(cty$NEW_HOSPITALIZED)]
    
    if(is.na(df[1,1])) {
      df[1,] <- c(names, new_hospitalizations)
    } else {
      df[nrow(df) + 1,] <- c(names, new_hospitalizations)
    }
    
  }
  
  df$new_hospitalizations <- as.numeric(df$new_hospitalizations)
  
  export <- list(counties = df$names, 
                 z = df$new_hospitalizations, 
                 type = "cmap", 
                 mtitle = paste("Daily COVID-19 Hospitalizations:", mcurr_date),
                 hovtext = "New hospitalizations:",
                 col1 = 'rgb(255, 255, 255)',
                 col2 = 'rgb(255, 204, 163)',
                 col3 = 'rgb(255, 175, 110)',
                 col4 = 'rgb(222, 134, 62)',
                 col5 = 'rgb(191, 86, 0)')
  
  return(export)
}

######################################
# State maps
######################################

total_cases_state_map <- function() {
  return(state_map(tail(daily_case_info_dataset$TOTAL_CASES, n = 1),
                  "Total cases:",
                  'rgb(105, 201, 245)',
                  "Total COVID-19 Cases"))
}

total_deaths_state_map <- function() {
  return(state_map(tail(daily_case_info_dataset$TOTAL_DEATHS, n = 1),
                   "New cases:",
                   'rgb(240, 101, 101)',
                   "Total COVID-19 Deaths"))
}

daily_cases_state_map <- function() {
  return(state_map(tail(daily_case_info_dataset$NEW_CASES, n = 1),
                   "New cases:",
                   'rgb(105, 201, 245)',
                   paste("New COVID-19 Cases:", mcurr_date)))
}

daily_deaths_state_map <- function() {
  return(state_map(tail(daily_case_info_dataset$NEW_DEATHS, n = 1),
                   "New deaths:",
                   'rgb(240, 101, 101)',
                   paste("New COVID-19 Deaths:", mcurr_date)))
}

testing_data_state_map <- function() {
  return(state_map(tail(daily_case_info_dataset$TOTAL_TESTS, n = 1),
                   "Total tests:",
                   'rgb(219, 122, 255)',
                   "Total COVID-19 Tests Performed"))
}

active_cases_state_map <- function() {
  return(state_map(tail(daily_case_info_dataset$TOTAL_ACTIVE, n = 1),
                   "Active cases:",
                   'rgb(105, 255, 142)',
                   "Active COVID-19 Cases"))
}

daily_active_cases_state_map <- function() {
  return(state_map(tail(daily_case_info_dataset$NEW_ACTIVE, n = 1),
                   "Net change of active cases:",
                   'rgb(105, 255, 142)',
                   paste("New Active COVID-19 Cases:", mcurr_date)))
}

total_recovered_state_map <- function() {
  return(state_map(tail(daily_case_info_dataset$TOTAL_RECOVERED, n = 1),
                   "Total recovered:",
                   'rgb(248, 255, 102)',
                   "Total COVID-19 Recoveries"))
}


daily_recovered_state_map <- function() {
  return(state_map(tail(daily_case_info_dataset$NEW_RECOVERED, n = 1),
                   "New recoveries:",
                   'rgb(248, 255, 102)',
                   paste("New COVID-19 Recoveries:", mcurr_date)))
}

total_hospitalizations_state_map <- function() {
  return(state_map(tail(daily_case_info_dataset$TOTAL_HOSP, n = 1),
                   "Total hospitalized:",
                   'rgb(255, 175, 110)',
                   "Total COVID-19 Hospitalizations"))
}

daily_hospitalizations_state_map <- function() {
  return(state_map(tail(daily_case_info_dataset$NEW_HOSP, n = 1),
                   "New hospitalizations:",
                   'rgb(255, 175, 110)',
                   paste("New COVID-19 Hospitalizations:", mcurr_date)))
}

######################################
# Abstract functions
######################################


total_plot <- function(xval, yval, linecolor, gtitle, ytitle) {
  fdate <- format(xval, "%Y-%m-%d")
  
  export <- list(xval = fdate, 
                 yval = yval, 
                 linecolor = linecolor, 
                 gtitle = gtitle, 
                 ytitle = ytitle,
                 type = "total")
  return(export)
}

daily_plot <- function(xval, yval, barcolor, fillcolor, movingAverage, movingLineColor, gtitle, ytitle) {
  fdate <- format(xval, "%Y-%m-%d")
  
  export <- list(xval = fdate, 
                 yval = yval, 
                 barcolor = barcolor, 
                 fillcolor = fillcolor, 
                 movingAverage = movingAverage,
                 movingLineColor = movingLineColor,
                 gtitle = gtitle, 
                 ytitle = ytitle,
                 type = "daily")
  return(export)
}

state_map <- function(z, hovtext, col, mtitle) {
  return(list(z = z,
              hovtext = hovtext,
              col = col,
              mtitle = mtitle,
              type = "smap"))
}

######################################
# State plots
######################################

total_cases_state_plot <- function() {
  return(total_plot(daily_case_info_dataset$DATE,
                    daily_case_info_dataset$TOTAL_CASES,
                    'rgb(0, 164, 179)',
                    "Total COVID-19 Cases in Tennessee",
                    "Total cases"))
}

total_deaths_state_plot <- function() {
  return(total_plot(daily_case_info_dataset$DATE,
                    daily_case_info_dataset$TOTAL_DEATHS,
                    'rgb(255, 93, 87)',
                    "Total COVID-19 Deaths in Tennessee",
                    "Total deaths"))
}

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

testing_data_state_plot <- function() {
  
}

active_cases_state_plot <- function() {
  return(total_plot(daily_case_info_dataset$DATE,
                    daily_case_info_dataset$TOTAL_ACTIVE,
                    'rgb(0, 219, 69)',
                    'Active COVID-19 Cases in Tennessee',
                    "Active cases"))
}

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

total_recovered_state_plot <- function() {
  return(total_plot(daily_case_info_dataset$DATE,
                    daily_case_info_dataset$TOTAL_RECOVERED,
                    'rgb(231, 235, 30)',
                    'Total Recovered COVID-19 Cases in Tennessee',
                    'Total recovered cases'))
}

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

total_hospitalizations_state_plot <- function() {
  return(total_plot(daily_case_info_dataset$DATE,
                    daily_case_info_dataset$TOTAL_HOSP,
                    'rgb(245, 99, 15)',
                    'Total COVID-19 Hospitalizations in Tennessee',
                    'Total hospitalizations'))
}

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

county_new_col_types <- c("date", "text", "numeric", "numeric","numeric", "numeric","numeric",
           "numeric","numeric","numeric","numeric","numeric",
           "numeric","numeric","numeric","numeric","numeric",
           "numeric","numeric", "numeric")

daily_case_info_col_types <- c("date", "numeric","numeric", "numeric", "numeric", "numeric", "numeric",
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
county_geojson <- rjson::fromJSON(file = "geojson/county_geojson.json")
tn_geojson <- rjson::fromJSON(file = 'geojson/tn_geojson.json')

datasets <- list(age_dataset,
                 county_new_dataset,
                 daily_case_info_dataset,
                 race_ethnicity_sex_dataset)

if(!all_same_date(datasets)) {
  stop("Datasets are not from the same day")
}

curr_date <- curr_dataset_date(datasets)
fcurr_date <- formatted_date(curr_date)
mcurr_date <- format(curr_date, "%m/%d/%Y")

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
                 "daily_cases", "daily_deaths", "testing", 
                 "active_cases", "daily_active", "total_recoveries", 
                 "daily_recoveries", "total_hospitalized", "daily_hospitalized")
      
    tcp <- total_cases_plot(county)
    tdp <- total_deaths_plot(county)
    
    dcp <- daily_cases_plot(county)
    ddp <- daily_deaths_plot(county)
    
    dtest <- daily_testing_data_plot(county)
    
    cac <- current_active_cases_plot(county)
    dac <- daily_active_cases_plot(county)
    
    tr <- total_recovered_plot(county)
    dr <- daily_recovered_plot(county)
    
    th <- total_hospitalized_plot(county)
    dh <- daily_hospitalizations_plot(county)
    
    plot_list <- list(tcp, tdp, dcp, ddp, dtest, cac, dac, tr, dr, th, dh)
    names(plot_list) <- l_names
    
    c_list <- append(c_list, list(plot_list))
  }
  
  names(c_list) <- c_name_list
  
  json_list <- toJSON(c_list)
  write(json_list, file = "counties.json")

  tcm <- total_cases_map(county_new_superlist)
  tdm <- total_deaths_map(county_new_superlist)
  dcm <- daily_cases_map(county_new_superlist)
  ddm <- daily_deaths_map(county_new_superlist)
  tm <- total_tests_map(county_new_superlist)
  acm <- current_active_cases_map(county_new_superlist)
  dacm <- daily_active_cases_map(county_new_superlist)
  trm <- total_recovered_map(county_new_superlist)
  drm <- daily_recovered_map(county_new_superlist)
  thm <- total_hospitalizations_map(county_new_superlist)
  dhm <- daily_hospitalizations_map(county_new_superlist)
   
  cmap_list <- list(tcm, tdm, dcm, ddm, tm, acm, dacm, trm, drm, thm, dhm)
  names(cmap_list) <- l_names
   
  json_cmap_list <- toJSON(cmap_list)
  write(json_cmap_list, file = 'cmaps.json')
  
  tcsm <- total_cases_state_map()
  tdsm <- total_deaths_state_map()
  dcsm <- daily_cases_state_map()
  ddsm <- daily_deaths_state_map()
  tsm <- testing_data_state_map()
  acsm <- active_cases_state_map()
  dacsm <- daily_active_cases_state_map()
  trsm <- total_recovered_state_map()
  drsm <- daily_recovered_state_map()
  thsm <- total_hospitalizations_state_map()
  dhsm <- daily_hospitalizations_state_map()
  
  smap_list <- list(tcsm, tdsm, dcsm, ddsm, tsm, acsm, dacsm, trsm, drsm, thsm, dhsm)
  names(smap_list) <- l_names
  
  json_smap_list <- toJSON(smap_list)
  write(json_smap_list, file = 'smaps.json')
  
  tcsp <- total_cases_state_plot()
  tdsp <- total_deaths_state_plot()
  dcsp <- daily_cases_state_plot()
  ddsp <- daily_deaths_state_plot()
  #tsp <- testing_state_plot()
  acsp <- active_cases_state_plot()
  dacsp <- daily_active_cases_state_plot()
  trsp <- total_recovered_state_plot()
  drsp <- daily_recovered_state_plot()
  thsp <- total_hospitalizations_state_plot()
  dhsp <- daily_hospitalizations_state_plot()
  
  state_plot_list <- list(tcsp, tdsp, dcsp, ddsp, list(), acsp, dacsp, trsp, drsp, thsp, dhsp)
  names(state_plot_list) <- l_names
  
  json_state_plots <- toJSON(state_plot_list)
  write(json_state_plots, file = 'state.json')
}

print("Successfully completed.")
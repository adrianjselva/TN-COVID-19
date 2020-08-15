#!/bin/env Rscript

######################################
# Libraries
######################################

library(readxl)
library(rjson)
library(plotly)
library(tools)
library(tidyr)

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

convert_date_race <- function(df, col_list) {
  df[[col_list[2]]] <- as.Date(df[[col_list[2]]])
  return(df)
}

load_file <- function(path, c_type) {
  f <- read_excel(path, col_types = c_type)
  f <- convert_date(f, colnames(f))
  f[is.na(f)] <- 0
  return(f)
}

load_file_race <- function(path, c_type) {
  f <- read_excel(path, col_types = c_type)
  f <- convert_date_race(f, colnames(f))
  f[is.na(f)] <- 0
  return(f)
}

recent_date <- function(df) {
  if(inherits(df[1,1][[1]], 'Date')) {
    return(df[nrow(df),1][[1]])
  } else {
    return(df[nrow(df),2][[1]])
  }
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

formatted_date <- function(d) {
  return(format(d, format = "%m-%d-%Y"))
}

dataset_path <- function(d) {
  return(paste("datasets", formatted_date(d), sep = "/"))
}

counties_path <- function(d) {
  return(paste("counties", formatted_date(d), sep = "/"))
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
  cnames <- c()

  for(c_name in unique(dataset$COUNTY)) {

    if(c_name == "Out of State") {
      next
    }

    c_data <- dataset[dataset$COUNTY == c_name,]
    f_code <- fips[fips$county == c_name,1]
    if(length(f_code) == 0) {
      f_code <- "0"
    }
    c_data$FIPS <- f_code
    c_data$COUNTY <- tolower(c_data$COUNTY)
    c_data$COUNTY <- tools::toTitleCase(c_data$COUNTY)

    if(c_data$COUNTY[1] == "Mcminn") {
      c_data$COUNTY <- rep("McMinn", length(c_data$COUNTY))
    }

    if(c_data$COUNTY[1] == "Mcnairy") {
      c_data$COUNTY <- rep("McNairy", length(c_data$COUNTY))
    }

    cnames <- c(cnames, c_data$COUNTY[1])

    c_data <- list(c_data)

    county_list <- append(county_list, c_data)
  }

  names(county_list) <- cnames

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

    if(col[i] != 0) {
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

add_new_positive_tests_cty <- function(super) {
  if("NEW_POS_TESTS" %in% names(super[[1]])) {
    return(super)
  }

  npt <- lapply(super, function(cty) {
    cty$NEW_POS_TESTS <- c(0, diff(cty$POS_TESTS))
    cty$NEW_POS_TESTS[(first_nonzero_index(cty$NEW_POS_TESTS))] <- 0

    pos_test_index <- which(colnames(cty) == "POS_TESTS")

    cty <- cty[, c(1:(pos_test_index), ncol(cty), (pos_test_index + 2):(ncol(cty) - 1))]

    return(cty)
  })


  return(npt)
}

add_new_positive_tests_state <- function(ds) {
  if("NEW_POS_TESTS" %in% colnames(ds)) {
    return(ds)
  }

  ds$NEW_POS_TESTS <- c(0, diff(ds$POS_TESTS))
  ds$NEW_POS_TESTS[(first_nonzero_index(ds$NEW_POS_TESTS))] <- 0

  pos_test_index <- which(colnames(ds) == "POS_TESTS")

  ds <- ds[, c(1:(pos_test_index), ncol(ds), (pos_test_index + 2):(ncol(ds) - 1))]

  return(ds)
}

rename_specimen_col <- function(sl) {
  nsl <- lapply(sl, function(cty) {
    colnames(cty)[colnames(cty) == "COUNTY_CASE_COUNT"] <- "NEW_CASES"

    cty <- cty %>%
      complete(DATE = seq.Date(min(DATE), max(DATE), by="day"), fill = list(NEW_CASES = 0, COUNTY = cty$COUNTY[1]))

    return(cty)
  })

  return(nsl)
}

create_state_specimen_collection <- function(ds) {
  nscds <- aggregate(ds["COUNTY_CASE_COUNT"], by=ds["DATE"], sum)
  colnames(nscds) <- c('DATE', "NEW_CASES")

  return(nscds)
}

######################################
# Abstract functions
######################################

source("plots/abstract_plots.R")
source("maps/abstract_maps.R")

######################################
# County Plots
######################################

source("plots/county/new_active_county.R")
source("plots/county/new_cases_county.R")
source("plots/county/new_cases_specimen_county.R")
source("plots/county/new_deaths_county.R")
source("plots/county/new_hospitalized_county.R")
source("plots/county/new_recovered_county.R")
source("plots/county/testing_county.R")
source("plots/county/total_active_county.R")
source("plots/county/total_cases_county.R")
source("plots/county/total_deaths_county.R")
source("plots/county/total_hospitalized_county.R")
source("plots/county/total_recovered_county.R")

source("plots/county/reported_cases_county.R")

######################################
# State plots
######################################

source("plots/state/new_active_state.R")
source("plots/state/new_cases_specimen_state.R")
source("plots/state/new_cases_state.R")
source("plots/state/new_deaths_state.R")
source("plots/state/new_hospitalized_state.R")
source("plots/state/new_recovered_state.R")
source("plots/state/testing_state.R")
source("plots/state/total_active_state.R")
source("plots/state/total_cases_state.R")
source("plots/state/total_deaths_state.R")
source("plots/state/total_hospitalized_state.R")
source("plots/state/total_recovered_state.R")

######################################
# County Maps
######################################

source("maps/county/new_active_county.R")
source("maps/county/new_cases_county.R")
#source("maps/county/new_cases_specimen_county.R")
source("maps/county/new_deaths_county.R")
source("maps/county/new_hospitalized_county.R")
source("maps/county/new_recovered_county.R")
source("maps/county/testing_county.R")
source("maps/county/total_active_county.R")
source("maps/county/total_cases_county.R")
source("maps/county/total_deaths_county.R")
source("maps/county/total_hospitalized_county.R")
source("maps/county/total_recovered_county.R")

######################################
# State maps
######################################

source("maps/state/new_active_state.R")
source("maps/state/new_cases_state.R")
source("maps/state/new_cases_specimen_state.R")
source("maps/state/new_deaths_state.R")
source("maps/state/new_hospitalized_state.R")
source("maps/state/new_recovered_state.R")
source("maps/state/testing_state.R")
source("maps/state/total_active_state.R")
source("maps/state/total_cases_state.R")
source("maps/state/total_deaths_state.R")
source("maps/state/total_hospitalized_state.R")
source("maps/state/total_recovered_state.R")

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
if(!dir.exists("output")) {
  dir.create("output")
}
dir.create("temp")

age_dataset_url <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-Age.XLSX"
age_county_dataset_url <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-Daily-County-Age-Group.XLSX"
county_new_dataset_url <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-County-New.XLSX"
daily_case_info_dataset_url <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-Daily-Case-Info.XLSX"
race_ethnicity_sex_dataset_url <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-RaceEthSex.XLSX"
county_school_dataset_url <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-Daily-County-Cases-5-18-Years.XLSX"
specimen_collection_dataset_url <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-MMWR-Week-Case-Count.XLSX"
age_outcomes_dataset_url <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-Daily-Age-Group-Outcomes.XLSX"

age_dataset_filename_prefix <- "age_dataset"
age_county_dataset_filename_prefix <- "age_county_dataset"
county_new_dataset_filename_prefix <- "county_new_dataset"
daily_case_info_dataset_filename_prefix <- "daily_case_info_dataset"
race_ethnicity_sex_dataset_filename_prefix <- "race_ethnicity_sex_dataset"
county_school_dataset_filename_prefix <- "county_school_dataset"
specimen_collection_filename_prefix <- "specimen_collection"
age_outcomes_filename_prefix <- "age_outcomes"

age_temp_path <- temp_path(add_xlsx(age_dataset_filename_prefix))
age_county_temp_path <- temp_path(add_xlsx(age_county_dataset_filename_prefix))
county_new_temp_path <- temp_path(add_xlsx(county_new_dataset_filename_prefix))
daily_case_info_temp_path <- temp_path(add_xlsx(daily_case_info_dataset_filename_prefix))
race_ethnicity_sex_temp_path <- temp_path(add_xlsx(race_ethnicity_sex_dataset_filename_prefix))
county_school_temp_path <- temp_path(add_xlsx(county_school_dataset_filename_prefix))
specimen_collection_temp_path <- temp_path(add_xlsx(specimen_collection_filename_prefix))
age_outcomes_temp_path <- temp_path(add_xlsx(age_outcomes_filename_prefix))

print("Downloading files...")
download.file(url = age_dataset_url, destfile = age_temp_path)
download.file(url = age_county_dataset_url, destfile = age_county_temp_path)
download.file(url = county_new_dataset_url, destfile = county_new_temp_path)
download.file(url = daily_case_info_dataset_url, destfile = daily_case_info_temp_path)
download.file(url = race_ethnicity_sex_dataset_url, destfile = race_ethnicity_sex_temp_path)
download.file(url = county_school_dataset_url, destfile = county_school_temp_path)
download.file(url = specimen_collection_dataset_url, destfile = specimen_collection_temp_path)
download.file(url = age_outcomes_dataset_url, destfile = age_outcomes_temp_path)

print("Loading files...")

age_col_types <- c("date", "text", "numeric", "numeric", "numeric",
                   "numeric", "numeric", "numeric")

age_county_col_types <- c("date", "text", "text", "numeric")

county_new_col_types <- c("date", "text", "numeric", "numeric", "numeric", "numeric","numeric", "numeric","numeric",
           "numeric","numeric","numeric","numeric","numeric",
           "numeric","numeric","numeric","numeric","numeric",
           "numeric","numeric", "numeric")

daily_case_info_col_types <- c("date", "numeric","numeric", "numeric", "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric", "numeric")

race_ethnicity_sex_col_types <- c("text", "date", "text", "numeric", "numeric", "numeric", "numeric")

county_school_col_types <- c("date", "text", "numeric", "numeric")

specimen_collection_col_types <- c("date", "text", "numeric", "numeric", "text")

age_outcomes_col_types <- c("date", "text", "numeric", "numeric", "numeric", "numeric", "numeric")

fips_col_types <- c("character", "character")

age_dataset <- load_file(age_temp_path, age_col_types)
age_county_dataset <- load_file(age_county_temp_path, age_county_col_types)
county_new_dataset <- load_file(county_new_temp_path, county_new_col_types)
daily_case_info_dataset <- load_file(daily_case_info_temp_path, daily_case_info_col_types)
race_ethnicity_sex_dataset <- load_file_race(race_ethnicity_sex_temp_path, race_ethnicity_sex_col_types)
county_school_dataset <- load_file(county_school_temp_path, county_school_col_types)
specimen_collection_dataset <- load_file(specimen_collection_temp_path, specimen_collection_col_types)
age_outcomes_dataset <- load_file(age_outcomes_temp_path, age_outcomes_col_types)

fips <- read.csv(file = 'fips/tn-counties.csv', stringsAsFactors = FALSE, colClasses = fips_col_types)
county_geojson <- rjson::fromJSON(file = "geojson/county_geojson.json")
tn_geojson <- rjson::fromJSON(file = 'geojson/tn_geojson.json')

datasets <- list(age_dataset,
                 age_county_dataset,
                 county_new_dataset,
                 daily_case_info_dataset,
                 race_ethnicity_sex_dataset,
                 county_school_dataset,
                 specimen_collection_dataset,
                 age_outcomes_dataset)

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
daily_case_info_dataset <- add_new_positive_tests_state(daily_case_info_dataset)

specimen_collection_superlist <- split_by_county(specimen_collection_dataset)
specimen_collection_superlist <- rename_specimen_col(specimen_collection_superlist)

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


msg <- paste("Creating plotly JSON files for ", fcurr_date, "...", sep = "")
print(msg)

c_list <- list()
c_name_list <- c()

for (county in county_new_superlist) {
  c_name <- county$COUNTY[1]
  c_name_list <- c(c_name_list, c_name)

  l_names <- c("total_cases", "total_deaths", "daily_cases", "daily_deaths",
               "testing", "active_cases", "daily_active", "total_recoveries",
               "daily_recoveries", "total_hospitalized", "daily_hospitalized",
               "daily_cases_specimen")
  
  lr_names <- c("total_cases", "total_deaths", "daily_cases", "daily_deaths",
               "testing", "active_cases", "daily_active", "total_recoveries",
               "daily_recoveries", "total_hospitalized", "daily_hospitalized",
               "daily_cases_specimen", "reported_cases")

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

  dcsp <- daily_cases_specimen_plot(specimen_collection_superlist[[c_name]])
  
  specimen_cases_county <- specimen_cases_plot_county(dcsp, tcsp)
  reported_cases_county <- reported_cases_plot_county(dcp, tcp)
  reported_deaths_county <- reported_deaths_plot_county(ddp, tdp)
  reported_active_county <- reported_active_plot_county(cac, dac)
  reported_recovered_county <- reported_recovered_plot_county(dr, tr)
  reported_hospitalized_county <- reported_hospitalized_plot_county(dh, th)
  

  plot_list <- list(tcp, tdp, dcp, ddp, dtest, cac, dac, tr, dr, th, dh, dcsp, reported_cases_county)
  names(plot_list) <- lr_names

  c_list <- append(c_list, list(plot_list))
}

names(c_list) <- c_name_list

json_list <- toJSON(c_list)
write(json_list, file = "output/county_plots.json")

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
dcsm <- daily_cases_map(specimen_collection_superlist)

cmap_list <- list(tcm, tdm, dcm, ddm, tm, acm, dacm, trm, drm, thm, dhm, dcsm)
names(cmap_list) <- l_names

json_cmap_list <- toJSON(cmap_list)
write(json_cmap_list, file = 'output/county_maps.json')

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
dcssm <- daily_cases_specimen_state_map()

smap_list <- list(tcsm, tdsm, dcsm, ddsm, tsm, acsm, dacsm, trsm, drsm, thsm, dhsm, dcssm)
names(smap_list) <- l_names

json_smap_list <- toJSON(smap_list)
write(json_smap_list, file = 'output/state_maps.json')

tcsp <- total_cases_state_plot()
tdsp <- total_deaths_state_plot()
dcsp <- daily_cases_state_plot()
ddsp <- daily_deaths_state_plot()
tsp <- testing_data_state_plot()
acsp <- active_cases_state_plot()
dacsp <- daily_active_cases_state_plot()
trsp <- total_recovered_state_plot()
drsp <- daily_recovered_state_plot()
thsp <- total_hospitalizations_state_plot()
dhsp <- daily_hospitalizations_state_plot()
dcssp <- daily_cases_specimen_state_plot(specimen_collection_dataset)

state_plot_list <- list(tcsp, tdsp, dcsp, ddsp, tsp, acsp, dacsp, trsp, drsp, thsp, dhsp, dcssp)
names(state_plot_list) <- l_names

json_state_plots <- toJSON(state_plot_list)
write(json_state_plots, file = 'output/state_plots.json')

date_json <- toJSON(list(date = format(curr_date, "%B %d, %Y")))
write(date_json, file = 'output/date.json')

print("Successfully completed.")

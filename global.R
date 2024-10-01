#-----------------------------------------------------------------------------------------------------------------
# Program: C:/GoogleDrive_MyDrive/scripts/R-Shinyapp_Strava-activity-data/global.R
# Date created: 27-SEP-2024
# Author(s): Lun-Hsien Chang
# Modified from: 
## C:/GoogleDrive_MyDrive/scripts/R-shinyapp_data-in-everyday-lives/global.R
## C:/GoogleDrive_MyDrive/scripts/R-shinyapp_data-in-everyday-lives/server.R
# Input: 
# Output: https://luenhchang.shinyapps.io/data-in-everyday-lives/
# References
## [How to Scrape and Store Strava Data Using R](https://rviews.rstudio.com/2021/11/22/strava-data/)
## [rStrava](https://fawda123.github.io/rStrava/)
## [dplyr r : selecting columns whose names are in an external vector [duplicate]](https://stackoverflow.com/questions/68749491/dplyr-r-selecting-columns-whose-names-are-in-an-external-vector)
## [Rounding duration or period to full minutes with lubridate](https://stackoverflow.com/questions/64929984/rounding-duration-or-period-to-full-minutes-with-lubridate)

## Date       Changes:
##--------------------------------------------------------------------------------------------------------------
## 2024-07-21 Run App locally with no problems. Deployment error: Error in gs4_auth() : Can't get Google credentials. Solved by adding googlesheets4::gs4_deauth() 
##------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------
# Load R packages
## Required uninstalled packages in local PC will cause errors library(pkg) is not available while deploying app to shinyapps.io
#-----------------------------------------------------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(dplyr)
#install.packages('rStrava')
library(rStrava)
library(googlesheets4)
library(lubridate)
library(DT)
library(plotly)
library(httr)
library(hms)

#------------------------------------------------------------------------
# Directory in local PC
## 
## www: Where all the images and other assets needed for the viewer
#------------------------------------------------------------------------
dir.C <- "C:"
dir.app <- file.path(dir.C, "GoogleDrive_MyDrive","scripts","R-Shinyapp_Strava-activity-data")
dir.data <- file.path(dir.app,"data")
# dir.www <- file.path(dir.app,"www")
# dir.create(path = dir.www)
# dir.img <- file.path(dir.www,"image_data-challenges")
# dir.img.annotated <- file.path(dir.www,"image_data-challenges-annotated") # dir.create(dir.img.annotated)
# dir.data <- file.path(dir.app,"data")
# dir.create(dir.data)

#------------------------------
# Scraping functions (no token)
#------------------------------
# Get athlete information 
# rStrava::athl_fun(athl_num=2837007, trace = FALSE)

#--------------------------------
# Read data from Strava using API 
#--------------------------------
# app_name <- 'ActivityTracker' # chosen by user
# app_client_id  <- '130546' # an integer, assigned by Strava
# app_secret <- '64ebf8746e6a8c0132dc63298cc74f7efb0207d4' # an alphanumeric secret, assigned by Strava

# Give Strava authentication. Create the authentication token file
## This step creates files .httr-oauth and .gitignore after the authentication is completed
## Authentication complete. Please close this page and return to R.
# stoken <- httr::config(token = rStrava::strava_oauth(
#   app_name=app_name
#   ,app_client_id=app_client_id
#   ,app_secret=app_secret
#   ,app_scope="activity:read_all"
#   ,cache = TRUE # Setting cache = TRUE for strava_oauth will create an authentication file .httr-oauth in the working directory
#   ))

# Read authentication file .httr-oauth from the app folder
#setwd(dir.app)
stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])
print(stoken) 
# <request>
#   Auth token: Token2.0

# My athlete id is 37641772
info.athlete.ID.37641772 <- rStrava::get_athlete(stoken, id ='37641772')
info.athlete.ID.37641772$id # 37641772
profile.start.date <- as.Date(info.athlete.ID.37641772$created_at)

# Get activities by date range
my_acts <- rStrava::get_activity_list( stoken
                                       ,after = profile.start.date #as.Date('2020-12-31')
) # class(my_acts) "list"

# User-defined function for NA
not_all_na <- function(x) any(!is.na(x)) # if all values are NA
not_any_na <- function(x) all(!is.na(x)) # if any value is NA

act_data <- rStrava::compile_activities(my_acts) |>
  # [Remove columns from dataframe where ALL values are NA](https://stackoverflow.com/questions/2643939/remove-columns-from-dataframe-where-all-values-are-na)
  dplyr::select(dplyr::where(not_all_na)) 
# class(act_data) [1] "actframe"   "data.frame" # dim(act_data) 897 52

#*****************************************
# Read data to use under menuItem "Swim" 
#*****************************************

# Subset pool swimming data
# Columns to include when subsetting a sport_type
common.columns <- c("id","name", "sport_type", "start_date_local","distance", "moving_time")

poolswim <- act_data |>
  dplyr::filter(grepl(pattern="^Indoor pool swim", x=name)) |>
  dplyr::select(dplyr::any_of(common.columns),average_heartrate, max_heartrate) |>
  # Format seconds to hh::mm::ss
  dplyr::mutate(moving_time_hhmmss=hms::as_hms(moving_time)) # dim(poolswim) 83 8

# Read single activity data
id.indoor.pool.swim.93 <- "12073687830"

library(httr)
library(jsonlite)
# Define the API endpoint
activity_id <- id.indoor.pool.swim.93

endpoint <- paste0("https://www.strava.com/api/v3/activities/", activity_id, "/streams")

# Make the request to fetch data
response <- GET(endpoint, 
                add_headers(Authorization = paste("Bearer", stoken)))

# Parse the JSON response
streams <- fromJSON(content(response, as = "text", encoding = "UTF-8"))

# Check the structure of the response
str(streams)

# Print error messages, if any
if ("errors" %in% names(streams)) {
  print(streams$errors)
}

if ("message" %in% names(streams)) {
  print(streams$message)
}

# Make the request
response <- httr::GET(endpoint, add_headers(Authorization = paste("Bearer", stoken)))

# Parse the JSON response
streams <- httr::content(response, as = "parsed", type = "application/json") # class(streams) "list"

# Convert JSON to a data frame (example for heart rate data)
heart_rate_data <- streams[[which(sapply(streams, function(x) x$type) == "heartrate")]]

# Access and print each component
for (stream in streams) {
  print(stream$type)
}

# Convert to data frame
heart_rate_df <- data.frame(
  time = heart_rate_data$time,
  heart_rate = heart_rate_data$data
)
# Fetch activity streams data
streams <- rStrava::get_activity_streams(id = id.indoor.pool.swim.93
                                         , types = c("heartrate", "time", "distance"))

activity_data <- rStrava::get_activity(id = id.indoor.pool.swim.93, stoken=stoken)

# Convert the list to a data frame
activity_df <- as.data.frame(t(unlist(activity_data)), stringsAsFactors = FALSE) # dim(activity_df) 1 87

# If there are nested lists, you might need to flatten them
stats_df <- as.data.frame(t(unlist(activity_data$stats)), stringsAsFactors = FALSE)

# Combine the main activity data with the nested stats data
activity_df <- bind_cols(activity_df, stats_df)

# Print the resulting data frame
print(activity_df)

#------------------------------------------------------------
# Read swim log from Google sheet
#------------------------------------------------------------
# Set Google sheet to Share > Anyone with the link > copy link
## [poolswim_log_responses.gsheet](https://docs.google.com/spreadsheets/d/1uAEniERik-uFUvWr8g4fMWDR50Okc_2mZCQNs-fSPFQ/edit?usp=sharing)
sheet_id <- "https://docs.google.com/spreadsheets/d/1uAEniERik-uFUvWr8g4fMWDR50Okc_2mZCQNs-fSPFQ/"

# Call `gs4_deauth()` to prevent the attempt to get credentials.
googlesheets4::gs4_deauth()

# Authentication complete. Please close this page and return to R.
# col_types: T for Datetime, c for character, n for numeric, D for date, t for Time of day (don't use t for duration)
poolswim.log <- googlesheets4::read_sheet(sheet_id
                                          ,col_types = "TcDncncc" ) |>
  as.data.frame() |>
  dplyr::mutate(
    # Convert character hh::mm::ss to Duration
    Corrected_moving_time_duration=lubridate::as.duration(lubridate::hms(Corrected_moving_time))
    ,Corrected_distance_100meters=Corrected_distance_meters/100
    ,pace_mmss_per100meters=Corrected_moving_time_duration/Corrected_distance_100meters
    ,pace_period=as.period(pace_mmss_per100meters)
    ,pace_minutes=as.integer(minute(pace_period))
    ,pace_seconds = as.integer(second(pace_period))
    ,pace_mmss_per100meters_fmt = sprintf("%02d:%02d", pace_minutes, pace_seconds)
    ,hovertext=paste(
      "Event Name :", Strava_activity_name, "\n"
      ,"Distance :", Corrected_distance_meters, "meters", "\n"
      ,"Time :", Corrected_moving_time, "\n"
      ,"Laps and training : \n"
      ,gsub(x=Laps_training, pattern=";", replacement="\n")
    ) # End paste()
  ) # dim(poolswim.log) 7 16

# Authentication complete. Please close this page and return to R.
poolswim.combined <- dplyr::left_join(
  x=poolswim.log
  ,y=poolswim[,c("name","start_date_local","average_heartrate", "max_heartrate")]
  ,by=c("Strava_activity_name"="name") ) # dim(poolswim.combined) 7 19

#*****************************************
# Read data to use under menuItem "Walk" 
#*****************************************
# Subset walking data
walk <- act_data |>
  dplyr::filter(sport_type=="Walk" & as.Date(start_date_local) >= as.Date("2024-01-01")) |>
  dplyr::select(dplyr::any_of(common.columns), total_elevation_gain) |>
  dplyr::mutate(
    # Date time to Date
    start_date=as.Date(start_date_local)
    # Convert seconds to lubridate duration
    ,moving_time_period=lubridate::seconds_to_period(moving_time)
                ,moving_time_duration=lubridate::as.duration(moving_time_period)
                # Calculate pace mm:ss/km from duration and distance and get minute part
                ,pace_minute_part=as.integer(lubridate::minute(lubridate::as.period(moving_time_duration/distance)))
                # Calculate pace mm:ss/km from duration and distance and get second part
                ,pace_second_part=as.integer(lubridate::second(lubridate::as.period(moving_time_duration/distance)))
                # Combine minute and second to a formatted mm::ss
                ,pace_mmss_per_km_fmt = sprintf("%02d:%02d", pace_minute_part, pace_second_part)
                # Customise hover text
                ,hovertext=paste(
                  "Event Name :", name, "(Event ID :", id, ")", "\n"
                  ,"Distance :", distance, "km", "\n"
                  ,"Moving time :", moving_time_period, "\n"
                  ,"Avg pace :", pace_mmss_per_km_fmt, "/km","\n")
                ) 
  # dim(walk) 54 14

# Read a single activity of Walk from Strava using id
## [Retrieve streams for Strava activities, and convert to a dataframe](https://rdrr.io/cran/rStrava/man/get_activity_streams.html)
# activity.11924685575 <- rStrava::get_activity_streams(
#   my_acts
#   ,stoken
#   ,id = 11924685575
#   ,types = NULL # types = NULL will get all columns
#   ) # class(activity.11924685575) [1] "strframe"   "data.frame" # dim(activity.11924685575) 8645 9

# hist(activity.11924685575$velocity_smooth)

#*****************************************
# Read data to use under menuItem "Run" 
#*****************************************
# Subset running data
run <- act_data |>
  dplyr::filter(sport_type=="Run" & as.Date(start_date_local) >= as.Date("2024-01-01")) |>
  dplyr::select(dplyr::any_of(common.columns), total_elevation_gain, average_heartrate, max_heartrate) |>
  dplyr::mutate(
    # Date time to Date
    start_date=as.Date(start_date_local)
    # Convert seconds to lubridate duration
    ,moving_time_period=lubridate::seconds_to_period(moving_time)
    ,moving_time_duration=lubridate::as.duration(moving_time_period)
    # Calculate pace mm:ss/km from duration and distance and get minute part
    ,pace_minute_part=as.integer(lubridate::minute(lubridate::as.period(moving_time_duration/distance)))
    # Calculate pace mm:ss/km from duration and distance and get second part
    ,pace_second_part=as.integer(lubridate::second(lubridate::as.period(moving_time_duration/distance)))
    # Combine minute and second to a formatted mm::ss
    ,pace_mmss_per_km_fmt = sprintf("%02d:%02d", pace_minute_part, pace_second_part)
    # Customise hover text
    ,hovertext=paste(
      "Event Name :", name, "(Event ID :", id, ")", "\n"
      ,"Distance :", distance, "km", "\n"
      ,"Moving time :", moving_time_period, "\n"
      ,"Avg pace :", pace_mmss_per_km_fmt, "/km","\n")
  ) # dim(run) 16 16

# Plot average speed by splits for a single activity interval run
# activity.11732270119 <- rStrava::get_spdsplits(
#   act_id=11732270119 
#   ,stoken
#   ,units = "metric") # dim(activity.11732270119) 3 3

#*****************************************
# Read data to use under menuItem "Ride" 
#*****************************************
# Subset running data
ride <- act_data |>
  dplyr::filter(sport_type=="Ride" & as.Date(start_date_local) >= as.Date("2024-01-01")) |>
  dplyr::select(dplyr::any_of(common.columns), average_speed, total_elevation_gain, average_heartrate, max_heartrate) |>
  dplyr::mutate(
    # Date time to Date
    start_date=as.Date(start_date_local)
    # Convert seconds to lubridate duration
    ,moving_time_period=lubridate::seconds_to_period(moving_time)
    ,moving_time_duration=lubridate::as.duration(moving_time_period)
    # Customise hover text
    ,hovertext=paste(
      "Event Name :", name, "\n"
      ,"Distance :", distance, "km", "\n"
      ,"Avg speed :", average_speed, "km/h","\n"
      ,"Elevation gain :", total_elevation_gain, "m"
      )
    ) # dim(ride) 49 16

# activity.11826580247 <- rStrava::get_activity_streams(
#   my_acts
#   ,stoken
#   ,id = 11826580247
#   ,types = NULL # types = NULL will get all columns
# ) |>
#   dplyr::mutate(pace_minutes=as.integer(minute(as.duration(1/velocity_smooth)*60*60))
#                 ,pace_seconds=as.integer(second(as.duration(1/velocity_smooth)*60*60))
#                 ,pace_mmss_per_km_fmt=sprintf("%02d:%02d", pace_minutes, pace_seconds)
#                 )
  # class(activity.11924685575) [1] "strframe"   "data.frame" # dim(activity.11826580247) 8281 9

#plot(activity.11826580247$velocity_smooth, type = "l")

# Get speed by splits for a single activity measured by speed/cadence sensor
# speed.splits.11826580247 <- rStrava::get_spdsplits(
#   act_id=11826580247
#   ,stoken
#   ,units = "metric") # dim(speed.splits.11826580247) 41 3

#-----------------------
# Analyse activities.csv
#-----------------------
# Download activities.csv from Google drive
## https://drive.google.com/file/d/1bx8iaRnjf4jATc4yWnB0-McLlDFpJZRP/view?usp=drive_link
df <- read.csv(file = file.path(dir.data,"activities.csv"), header = TRUE) # dim(df) 657 87

format.datetime <- "%b %d, %Y, %I:%M:%S %p"

activities.df <- df |> 
  dplyr::select(Activity.ID, Activity.Date, Activity.Name, Activity.Gear, Activity.Type, Distance, Elevation.Gain, Elapsed.Time, Moving.Time, Max.Heart.Rate) |>
  dplyr::mutate(
    start.datetime.UTC=as.POSIXct(x=Activity.Date, format = format.datetime, tz="UTC")
    ,start.date.UTC=lubridate::date(start.datetime.UTC)
    ,start.datetime.local=case_when(
      start.date.UTC >= ymd('2023-01-16') & start.date.UTC <= ymd('2023-02-05')~
        as.POSIXct(start.datetime.UTC, tz="Asia/Kuala_Lumpur")
      ,start.date.UTC >= ymd('2023-09-02') & start.date.UTC <= ymd('2023-10-02') ~ as.POSIXct(start.datetime.UTC, tz="Asia/Taipei")
      ,TRUE ~ as.POSIXct(start.datetime.UTC, tz="Australia/Brisbane")
    )
    ,start.date.local=lubridate::date(start.datetime.local)
    ,start.year.local=lubridate::year(start.datetime.local)
    ,start.dayofyear.local=lubridate::yday(start.datetime.local)
    ,start.month.local=lubridate::month(start.datetime.local)
    ,start.day.local=weekdays(start.datetime.local)
    ,start.week.local=lubridate::week(start.datetime.local)
    # Distance contains decimal places for cycling running distance, and comma for swim distance
    ,distance.km= dplyr::case_when(
      Activity.Type=="Swim" ~ as.numeric(gsub(pattern=",", replacement=".", x=Distance))
      ,Activity.Type %in% c("Ride","Run","E-Bike Ride", "Walk") ~ as.numeric(Distance)
      ,TRUE ~ NA_integer_)
    ,elevation.gain.m= Elevation.Gain # Elevation gain in meters
    ,elapsed.time.hour=Elapsed.Time/60/60
    ,moving.time.hour=Moving.Time/60/60) |>
  dplyr::select(-Distance, -Elevation.Gain, -Elapsed.Time, -Moving.Time) # dim(activities.df) 657 19


#************************************************************************************************#
#---------------------------------This is the end of this file ----------------------------------#
#************************************************************************************************#
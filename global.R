#----------------------------------------------------------------------------------------------------------
# Program: C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_Strava-activity-data/global.R
# Modified from: C:/GoogleDrive_MyDrive/scripts/R-Shinyapp_Strava-activity-data/global.R
# Date created: 01-OCT-2024
# Author(s): Lun-Hsien Chang
# Input: 
# Output: 
# References
## [How to Scrape and Store Strava Data Using R](https://rviews.rstudio.com/2021/11/22/strava-data/)
## [rStrava](https://fawda123.github.io/rStrava/)
## [dplyr r : selecting columns whose names are in an external vector [duplicate]](https://stackoverflow.com/questions/68749491/dplyr-r-selecting-columns-whose-names-are-in-an-external-vector)
## [Rounding duration or period to full minutes with lubridate](https://stackoverflow.com/questions/64929984/rounding-duration-or-period-to-full-minutes-with-lubridate)
## [Converting UTC time to local standard time in R](https://stackoverflow.com/questions/31325072/converting-utc-time-to-local-standard-time-in-r)
## [R - Storing plotly objects inside a list](https://stackoverflow.com/questions/54466628/r-storing-plotly-objects-inside-a-list)
## [plotly::sublot not showing both titles](https://stackoverflow.com/questions/68796762/plotlysublot-not-showing-both-titles)
## [Missing Git tab in Rstudio on Windows computer](https://mikenguyen.netlify.app/post/missing-git-tab-in-rstudio-on-windows-computer/)
## [Specifying the colors in a Plotly Heatmap](https://stackoverflow.com/questions/44861851/specifying-the-colors-in-a-plotly-heatmap)
## Date       Changes:
##---------------------------------------------------------------------------------------------------------
## 2024-10-04 Git pane disappeared when opening this R file. Git pane appearred after on the top right corner change project (none) to RProject_Shinyapp_Strava-activity-data
##---------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------
# Load R packages
## Required uninstalled packages in local PC will cause errors library(pkg) is not available while deploying app to shinyapps.io
#----------------------------------------------------------------------------------------------------------
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
library(httr2)
library(hms)
library(jsonlite)
library(pals)
library(rsconnect)
library(usethis)
library(viridis)

#------------------------------------------------------------------------
# Directory in local PC
## www: Where all the images and other assets needed for the viewer
#------------------------------------------------------------------------
dir.C <- "C:"
dir.app <- file.path(dir.C, "GoogleDrive_MyDrive","scripts","RProject_Shinyapp_Strava-activity-data")
dir.data <- file.path(dir.app,"data")
dir.fitness <- file.path(dir.C, "GoogleDrive_MyDrive","Fitness")
dir.Strava <- file.path(dir.fitness,"Strava")
dir.Strava.export_37641772 <- file.path(dir.Strava,"export_37641772")

#setwd(dir.app)

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
# class(act_data) [1] "actframe"   "data.frame" # dim(act_data) 922 52

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
  dplyr::mutate(moving_time_hhmmss=hms::as_hms(moving_time)) # dim(poolswim) 98 8

#--------------------------
# Read single activity data
#--------------------------
id.indoor.pool.swim.93 <- "12073687830"

activity_data <- rStrava::get_activity(id = id.indoor.pool.swim.93, stoken=stoken)

# Convert the list to a data frame
activity_df <- as.data.frame(t(unlist(activity_data)), stringsAsFactors = FALSE) # dim(activity_df) 1 87

# If there are nested lists, you might need to flatten them
stats_df <- as.data.frame(t(unlist(activity_data$stats)), stringsAsFactors = FALSE)

# Combine the main activity data with the nested stats data
activity_df <- bind_cols(activity_df, stats_df)

# Print the resulting data frame
#print(activity_df)

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
  ,by=c("Strava_activity_name"="name") ) # dim(poolswim.combined) 22 19

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
                ) # dim(walk) 69 14

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
  ) # dim(run) 17 16

# Plot average speed by splits for a single activity interval run
# activity.11732270119 <- rStrava::get_spdsplits(
#   act_id=11732270119 
#   ,stoken
#   ,units = "metric") # dim(activity.11732270119) 3 3

#**************************
# Process activity data
#**************************
# 2024-10-01T02:35:06Z is in UTC. The "Z" at the end stands for Zulu time, which is another way to indicate UTC (Coordinated Universal Time).
act_data.1 <- act_data %>%
  dplyr::select(id, start_date, name, gear_id, sport_type, distance, total_elevation_gain, elapsed_time, moving_time,average_heartrate, max_heartrate) %>%
  dplyr::mutate(
     start.datetime.UTC=as.POSIXct(x=start_date, format ="%Y-%m-%dT%H:%M:%SZ", tz="GMT")
    ,start.date.UTC=lubridate::date(start.datetime.UTC)
    ,start.datetime.local=case_when(
      start.date.UTC >= ymd('2023-01-16') & start.date.UTC <= ymd('2023-02-05')~
        format(start.datetime.UTC, tz="Asia/Kuala_Lumpur",usetz=TRUE)
        #as.POSIXct(start.datetime.UTC, tz="Asia/Kuala_Lumpur")
      ,start.date.UTC >= ymd('2023-09-02') & start.date.UTC <= ymd('2023-10-02') ~ 
        #as.POSIXct(start.datetime.UTC, tz="Asia/Taipei")
        format(start.datetime.UTC, tz="Asia/Taipei",usetz=TRUE)
      #,TRUE ~ as.POSIXct(start.datetime.UTC, tz="Australia/Brisbane")
      ,TRUE ~ format(start.datetime.UTC, tz="Australia/Brisbane",usetz=TRUE)
      )
    ,start.date.local=lubridate::date(start.datetime.local)
    ,start.year.local=lubridate::year(start.datetime.local)
    ,start.dayofyear.local=lubridate::yday(start.datetime.local)
    ,start.month.local=lubridate::month(start.datetime.local)
    ,start.weekday.local=weekdays(as.Date(start.datetime.local))
    ,start.week.local=lubridate::week(as.Date(start.datetime.local))
    ,distance.km=distance
    ,elevation.gain.m= total_elevation_gain # Elevation gain in meters
    ,elapsed.time.hour=elapsed_time/60/60
    ,moving.time.hour= moving_time/60/60) %>%
  dplyr::select(-distance, -total_elevation_gain) # dim(act_data.1) 924 22

#*****************************************
# Read data to use under menuItem "Ride" 
#*****************************************
# Subset running data
ride <- act_data.1 |>
  dplyr::select(id, name, sport_type, start.year.local, start.week.local, start.date.local, start.weekday.local,start.dayofyear.local, distance.km, moving_time, moving.time.hour, elevation.gain.m, average_heartrate, max_heartrate) |>
  dplyr::filter(sport_type=="Ride") |>
  dplyr::mutate(
    # Convert seconds to lubridate duration
    ,moving_time_period=lubridate::seconds_to_period(moving_time)
    ,moving_time_duration=lubridate::as.duration(moving_time_period)) # dim(ride) 321 16

# Read single activity (not working)
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

# Calculate cycling elevation gain and distance per day
ride.day <- ride %>%
  dplyr::group_by(start.year.local, start.week.local,start.dayofyear.local,start.date.local, start.weekday.local) %>%
  dplyr::summarise(distance.km.day= sum(distance.km)
                   ,elevation.gain.m.day=sum(elevation.gain.m)) %>%
  # Accumulate distance, elevation gain yearly
  dplyr::group_by(start.year.local) %>%
  dplyr::arrange(start.date.local) %>%
  dplyr::mutate(ride.distance.cum.year = cumsum(distance.km.day)
                ,ride.elevation.cum.year = cumsum(elevation.gain.m.day)) # dim(ride.day) 307 9

# Calculate total ride distance, elevation per weekday, year
ride.weekday.year <- ride %>%
  # Collapse cycling data to 1 record per weekday (e.g., Monday, Tuesday,...) and year
  dplyr::group_by(start.year.local, start.weekday.local) %>%
  dplyr::summarise(count.ride=dplyr::n()
                   ,total.ride.distance.dayOfWeek= round(sum(distance.km), digits = 2)
                   ,total.ride.elevation.dayOfWeek=round(sum(elevation.gain.m), digits = 2)
                   ) # dim(ride.weekday.year) 31 5

#-----------------------------------------------------
# Find weekday with greatest number of rides in a year
#-----------------------------------------------------
ride.year.highest.ride.count.weekday <- ride.weekday.year %>%
  # Collapse cycling data to 1 record per year
  dplyr::group_by(start.year.local) %>%
  dplyr::filter(count.ride==max(count.ride)) %>%
  # Customise text following infobox title "The most active weekday in"
  dplyr::mutate(infobox.value = paste0(start.year.local, " : ", start.weekday.local, 
                                       " (", count.ride, " rides)")) # dim(ride.year.highest.ride.count.weekday) 5 6 # ride.year.highest.ride.count.weekday$infobox.value

# Collapse all values into a single string with <br/> for line breaks
infobox.value.yearly.weekday.highest.ride.number <- HTML(paste(ride.year.highest.ride.count.weekday$infobox.value, collapse = "<br/>"))

#-----------------------------------------------------
# Find weekday with longest cycling distance in a year
#-----------------------------------------------------
ride.year.longest.ride.distance.weekday <- ride.weekday.year %>%
  # Collapse cycling data to 1 record per year
  dplyr::group_by(start.year.local) %>%
  dplyr::filter(total.ride.distance.dayOfWeek==max(total.ride.distance.dayOfWeek)) %>%
  # Customise text following infobox title "The most active weekday in"
  dplyr::mutate(infobox.value = paste0(start.year.local, " : ", start.weekday.local, 
                                       " (", total.ride.distance.dayOfWeek, " km)")) # dim(ride.year.longest.ride.distance.weekday) 5 6

# Collapse all values into a single string with <br/> for line breaks
infobox.value.yearly.weekday.longest.ride.distance <- HTML(paste(ride.year.longest.ride.distance.weekday$infobox.value, collapse = "<br/>"))

#--------------------------------------------------------
# Find weekday with best cycling elevation gain in a year
#--------------------------------------------------------
ride.year.greatest.ride.elevation.weekday <- ride.weekday.year %>%
  # Collapse cycling data to 1 record per year
  dplyr::group_by(start.year.local) %>% 
  dplyr::filter(total.ride.elevation.dayOfWeek==max(total.ride.elevation.dayOfWeek)) %>%
  # Customise text following infobox title "The most active weekday in"
  dplyr::mutate(infobox.value = paste0(start.year.local, " : ", start.weekday.local, 
                                       " (", total.ride.elevation.dayOfWeek, " m)")) # dim(ride.year.greatest.ride.elevation.weekday) 5 6

# Collapse all values into a single string with <br/> for line breaks
infobox.value.yearly.weekday.greatest.ride.elevation <- HTML(paste(ride.year.greatest.ride.elevation.weekday$infobox.value, collapse = "<br/>"))

#**********************************************
# Read data to use under menuItem "Active time" 
#**********************************************

#------------------
# Process 2023 data 
#------------------
activities.2023 <- act_data.1 %>%
  dplyr::filter(start.year.local==2023) %>%
  # Split sport_type="Workout"
  dplyr::mutate(activity.type=dplyr::case_when(
     grepl(pattern="table tennis", x=name, ignore.case=TRUE) ~ stringi::stri_trans_totitle("table tennis")
    ,grepl(pattern="badminton", x=name, ignore.case=TRUE) ~ stringi::stri_trans_totitle("badminton")
    ,grepl(pattern="rehabilitation exercise|strength and stability exercises|dry land exercises", x=name, ignore.case=TRUE) ~ stringi::stri_trans_totitle("strength & stability workout")
    ,grepl(pattern="bike fitting", x=name, ignore.case=TRUE) ~ stringi::stri_trans_totitle("bike fitting")
    ,TRUE ~ sport_type )
    ) # dim(activities.2023) 404 23

#------------------
# Process 2024 data
#------------------
activities.2024 <- act_data.1 %>%
  dplyr::filter(start.year.local==2024) %>%
  dplyr::mutate(activity.type=sport_type) # dim(activities.2024) 218 21

data.moving.time.2024 <- activities.2024 %>%  
  dplyr::filter(!is.na(moving.time.hour) & activity.type !="EBikeRide") # dim(data.moving.time.2024) 207 21

#-----------------------------------------------------------
# Check which shinyapps.io account is used before deployment
#-----------------------------------------------------------
#rsconnect::accountInfo()
# $name
# [1] "luenhchang"
# 
# $server
# [1] "shinyapps.io"
# 
# $accountId
# [1] "981190"
# 
# $token
# [1] "A9C5D75F89CEC1B84E8F1CCFEEBB5CF1"
# 
# $secret
# [1] "W10qQN... (redacted)"
# 
# $username
# [1] "luenhchang"
#************************************************************************************************#
#---------------------------------This is the end of this file ----------------------------------#
#************************************************************************************************#
#----------------------------------------------------------------------------------------------------------
# Program: C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_Strava-activity-data/global.R
# Modified from: C:/GoogleDrive_MyDrive/scripts/R-Shinyapp_Strava-activity-data/global.R
# Date created: 01-OCT-2024
# Author(s): Lun-Hsien Chang
# Input: 
# Output: https://luenhchang.shinyapps.io/Strava-activity-data/
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
## 2024-12-31 Lawn mowing included in top left-aligned legend of Activity hours in 2024 plot
## 2024-12-31 Added Lawn mowing in activity.type.
## 2024-12-19 Moved all customed functions to functions.R
## 2024-11-27 Moved shinyapps.io account checking to check_shinyapps_io_accounts.R
## 2024-11-27 Moved Strava credentials to Strava_API_credentials.R
## 2024-11-04 Deployment completed: https://luenhchang.shinyapps.io/Strava-activity-data/
## 2024-10-29 Successfully deployed to <https://luenhchang.shinyapps.io/Strava-activity-data/> Deployment completed: https://luenhchang.shinyapps.io/Strava-activity-data/
## 2024-10-22 Successfully deployed to <https://luenhchang.shinyapps.io/Strava-activity-data/> Deployment completed: https://luenhchang.shinyapps.io/Strava-activity-data/
## 2024-10-06 Deployed app
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

#----------------------------------
# Import functions from functions.R
#----------------------------------
#setwd(dir.app)
source("functions.R")

#------------------------------
# Scraping functions (no token)
#------------------------------
# Get athlete information 
# rStrava::athl_fun(athl_num=2837007, trace = FALSE)

#--------------------------------
# Read data from Strava using API 
#--------------------------------

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

act_data <- rStrava::compile_activities(my_acts) |>
  # [Remove columns from dataframe where ALL values are NA](https://stackoverflow.com/questions/2643939/remove-columns-from-dataframe-where-all-values-are-na)
  dplyr::select(dplyr::where(not_all_na)) 
# class(act_data) [1] "actframe"   "data.frame" # dim(act_data) 955 52

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
    ,pace_period=lubridate::as.period(pace_mmss_per100meters)
    ,pace_minutes=as.integer(lubridate::minute(pace_period))
    ,pace_seconds = as.integer(lubridate::second(pace_period))
    ,pace_mmss_per100meters_fmt = sprintf("%02d:%02d", pace_minutes, pace_seconds)
    ,hovertext=paste(
      "Event Name :", Strava_activity_name, "\n"
      ,"Distance :", Corrected_distance_meters, "meters", "\n"
      ,"Time :", Corrected_moving_time, "\n"
      ,"Laps and training : \n"
      ,gsub(x=Laps_training, pattern=";", replacement="\n")
    ) # End paste()
  ) # dim(poolswim.log) 23 16

# Authentication complete. Please close this page and return to R.
poolswim.combined <- dplyr::left_join(
  x=poolswim.log
  ,y=poolswim[,c("name","start_date_local","average_heartrate", "max_heartrate")]
  ,by=c("Strava_activity_name"="name") ) # dim(poolswim.combined) 23 19

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
                ) # dim(walk) 73 14

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
  ) # dim(run) 26 16

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
      start.date.UTC >= lubridate::ymd('2023-01-16') & start.date.UTC <= lubridate::ymd('2023-02-05')~
        format(start.datetime.UTC, tz="Asia/Kuala_Lumpur",usetz=TRUE)
      ,start.date.UTC >= lubridate::ymd('2023-09-02') & start.date.UTC <= lubridate::ymd('2023-10-02') ~ 
        format(start.datetime.UTC, tz="Asia/Taipei",usetz=TRUE)
      ,TRUE ~ format(start.datetime.UTC, tz="Australia/Brisbane",usetz=TRUE)
      )
    ,start.date.local=lubridate::date(start.datetime.local)
    # get the day of the year (e.g. Returns 1 for 1st Jan, Returns 366 for 31st Dec in a leap year)
    ,start.date.local.day.of.year=lubridate::yday(start.date.local)
    ,start.year.local=lubridate::year(start.datetime.local)
    ,start.dayofyear.local=lubridate::yday(start.datetime.local)
    ,start.month.local=lubridate::month(start.datetime.local)
    ,start.weekday.local=weekdays(as.Date(start.datetime.local))
    ,start.week.local=lubridate::week(as.Date(start.datetime.local))
    ,distance.km=distance
    ,elevation.gain.m= total_elevation_gain # Elevation gain in meters
    ,elapsed.time.hour=elapsed_time/60/60
    ,moving.time.hour= moving_time/60/60) %>%
  dplyr::select(-distance, -total_elevation_gain) # dim(act_data.1) 955 23

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
    ,moving_time_duration=lubridate::as.duration(moving_time_period)) # dim(ride) 327 16

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
                   ,elevation.gain.m.day=sum(elevation.gain.m)
                   # Combine multiple activities a day to a string with events separated by comma
                   ,activity.name.day=paste(name, collapse = ", ")
                   ,num.ride.day=dplyr::n()
                   ) %>%
  # Accumulate distance, elevation gain yearly
  dplyr::group_by(start.year.local) %>%
  dplyr::arrange(start.date.local) %>%
  dplyr::mutate(ride.distance.cum.year = cumsum(distance.km.day)
                ,ride.elevation.cum.year = cumsum(elevation.gain.m.day)) # dim(ride.day) 313 11

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
infobox.value.yearly.weekday.highest.ride.number <- htmltools::HTML(paste(ride.year.highest.ride.count.weekday$infobox.value, collapse = "<br/>"))

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
infobox.value.yearly.weekday.longest.ride.distance <- htmltools::HTML(paste(ride.year.longest.ride.distance.weekday$infobox.value, collapse = "<br/>"))

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
infobox.value.yearly.weekday.greatest.ride.elevation <- htmltools::HTML(paste(ride.year.greatest.ride.elevation.weekday$infobox.value, collapse = "<br/>"))

#**********************************************
# Read data to use under menuItem "Active time" 
#**********************************************

#--------------------------------------------------------------------------------------------
# Find months that are most or least active in a year
## Activeness is number of unique exercise days divided by number of days in the month * 100%
#--------------------------------------------------------------------------------------------
monthly.activeness.percentage <- act_data.1 %>%
  dplyr::filter(start.year.local >= 2021) %>%
  # Group by year and month
  dplyr::group_by(start.year.local, start.month.local) %>%
  # Summarize to one row per group
  dplyr::summarise(
    unique_days = dplyr::n_distinct(start.date.local), # Count unique exercise days
    .groups = "drop" # Drop grouping structure to ensure clarity
  ) %>%
  # Add days_in_month calculation after summarizing
  dplyr::mutate(
    days_in_month = lubridate::days_in_month(as.Date(paste(start.year.local, start.month.local, "01", sep = "-"))),
    percent.days.active = (unique_days / days_in_month) * 100
  ) # dim(monthly.activeness.percentage) [1] 51  5

most.active.months <- monthly.activeness.percentage %>%
  # Group by year
  dplyr::group_by(start.year.local) %>%
  # Get the record with the highest percent.days.active per year
  dplyr::slice_max(percent.days.active, n = 1) %>%
  # Ungroup to remove grouping structure
  dplyr::ungroup() %>%
  dplyr::mutate(
    # Convert numeric month to month name
    start.month.local.name = lubridate::month(start.month.local, label = TRUE, abbr = FALSE)
    ,infobox.value= paste0(start.year.local
                           , " : "
                           , start.month.local.name
                           ," ("
                           , round(percent.days.active, digits = 2)
                           , "% of days with exercise)")
  )# dim(most.active.months) [1] 6 7

# Collapse all values into a single string with <br/> for line breaks
infobox.value.most.active.months <- htmltools::HTML(paste(most.active.months$infobox.value, collapse = "<br/>"))

least.active.months <- monthly.activeness.percentage %>%
  # Group by year
  dplyr::group_by(start.year.local) %>%
  # Get the record with the highest percent.days.active per year
  dplyr::slice_min(percent.days.active, n = 1) %>%
  # Ungroup to remove grouping structure
  dplyr::ungroup() %>%
  dplyr::mutate(
    # Convert numeric month to month name
    start.month.local.name = lubridate::month(start.month.local, label = TRUE, abbr = FALSE)
    ,infobox.value= paste0(start.year.local
                           , " : "
                           , start.month.local.name
                           ," ("
                           , round(percent.days.active, digits = 2)
                           , "% of days with exercise)")
  )# dim(most.active.months) [1] 6 7

# Collapse all values into a single string with <br/> for line breaks
infobox.value.least.active.months <- htmltools::HTML(paste(least.active.months$infobox.value, collapse = "<br/>"))

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
    ) # dim(activities.2023) 404 24

#------------------
# Process 2024 data
#------------------
activities.2024 <- act_data.1 %>%
  dplyr::filter(start.year.local==2024) %>%
  dplyr::arrange(start.datetime.local) %>%
  dplyr::mutate(activity.type= 
                  dplyr::case_when(
                    grepl(pattern = "lawn mow", x=name, ignore.case = TRUE) ~ "Lawn mowing"
                    ,grepl(pattern = "gardening", x=name, ignore.case = TRUE) ~ "Gardening"
                    ,TRUE ~ sport_type)
                )# dim(activities.2024) 249 24

data.moving.time.2024 <- activities.2024 %>%  
  dplyr::filter(!is.na(moving.time.hour) & activity.type !="EBikeRide") # dim(data.moving.time.2024) 235 24

#------------------
# Process 2025 data
#------------------
activities.2025 <- act_data.1 %>%
  dplyr::filter(start.year.local==2025) %>%
  dplyr::arrange(start.datetime.local) %>%
  dplyr::mutate(activity.type= 
                  dplyr::case_when(
                    grepl(pattern = "lawn mow", x=name, ignore.case = TRUE) ~ "Lawn mowing"
                    ,grepl(pattern = "gardening", x=name, ignore.case = TRUE) ~ "Gardening"
                    ,TRUE ~ sport_type)
  )# dim(activities.2025) 1 24

data.moving.time.2025 <- activities.2025 %>%  
  dplyr::filter(!is.na(moving.time.hour) & activity.type !="EBikeRide") # dim(data.moving.time.2025) 1 24

#-------------------------------------------------------------------
# Create data for this week progress dashboard like Strava's in 2024
#-------------------------------------------------------------------
# Get the current date
current_date <- Sys.Date()

# Calculate the start of this week (Sunday) and last week (Sunday of the previous week)
start_of_this_week <- current_date - as.integer(format(current_date, "%u")) + 1
start_of_last_week <- start_of_this_week - 7

# Extract this week's data
this_week_stats <- act_data.1 %>%
  filter(start.date.local >= start_of_this_week) %>%
  summarise(
    total_distance_km = sum(distance.km, na.rm = TRUE)
    ,total_moving_time_hour = sum(moving.time.hour, na.rm = TRUE)
    ,number_activities = n()  # Count the number of activities
  ) %>%
  replace(is.na(.), 0)

# Reshape data to long format
this_week_stats_long <- this_week_stats %>% 
  tidyr::pivot_longer(
    cols = c(total_distance_km, total_moving_time_hour, number_activities)
    ,names_to = "metric"
    ,values_to = "value") %>%
  mutate(week = "this")

# Extract last week's data
last_week_stats <- act_data.1 %>%
  filter(start.date.local >= start_of_last_week & start.date.local < start_of_this_week) %>%
  summarise(
    total_distance_km = sum(distance.km, na.rm = TRUE)
    ,total_moving_time_hour = sum(moving.time.hour, na.rm = TRUE)
    ,number_activities = n()  # Count the number of activities
  ) %>%
  replace(is.na(.), 0)

# Reshape data to long format
last_week_stats_long <- last_week_stats %>%
  tidyr::pivot_longer(
    cols = c(total_distance_km, total_moving_time_hour, number_activities)
    ,names_to = "metric"
    ,values_to = "value") %>%
  mutate(week = "last")

# Merge this week long and last week long
stats <- merge(x=this_week_stats_long
               ,y=last_week_stats_long
               ,by = "metric"
               ,suffixes = c("_this", "_last")) %>%
  # Calculate percentage change between this and last week
  dplyr::mutate(change= round((value_this-value_last)/value_last *100
                              , digits = 2)
  )

# Convert to lists for use in valueBoxes
this_week_list <- split(stats$value_this, stats$metric)
change_list <- split(stats$change, stats$metric)

#************************************************************************************************#
#---------------------------------This is the end of this file ----------------------------------#
#************************************************************************************************#
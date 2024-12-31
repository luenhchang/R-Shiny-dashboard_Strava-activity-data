#----------------------------------------------------------------------------------------------------------
# Program: C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_Strava-activity-data/export_Strava_activity_data.R
# Date created: 20-DEC-2024
# Author(s): Lun-Hsien Chang
# Modified from:
# Purposes: Export strava activity data to tsv, 1 year per file. Data from current year and previous years are processed separately.
## Date       Changes:
##---------------------------------------------------------------------------------------------------------
## 2024-12-20 Moved customed functions from global.R, server.R to here
##---------------------------------------------------------------------------------------------------------

library(dplyr)
library(readr)
library(googledrive)

# Authenticate with Google Drive
drive_auth()

# Specify the Google Drive folder ID
folder_id <- "1k495O3mJw56Vv5ldDI3C7yZK_JyAUB6A"

# Define a function to read TSV files with consistent column types
read_strava_data <- function(file_path) {
  read_tsv(file_path, col_types = cols(
    id = col_character(),                    # Ensure 'id' is character
    elapsed_time = col_double(),             # Ensure 'elapsed_time' is double
    moving_time = col_double(),              # Ensure 'moving_time' is double
    average_heartrate = col_double(),        # Ensure 'average_heartrate' is double
    max_heartrate = col_double(),            # Ensure 'max_heartrate' is double
    start_date = col_datetime(format = ""),  # Ensure 'start_date' is datetime
    start.datetime.UTC = col_datetime(format = ""),     # Ensure 'start.datetime.UTC' is datetime
    start.date.UTC = col_date(),             # Ensure 'start.date.UTC' is date
    start.date.local = col_date(),           # Ensure 'start.date.local' is date
    start.date.local.day.of.year = col_double(), # Ensure 'start.date.local.day.of.year' is numeric
    start.year.local = col_double(),         # Ensure 'start.year.local' is numeric
    start.dayofyear.local = col_double(),    # Ensure 'start.dayofyear.local' is numeric
    start.month.local = col_character(),     # Ensure 'start.month.local' is character (change as needed)
    start.week.local = col_double(),         # Ensure 'start.week.local' is numeric
    distance.km = col_double(),              # Ensure 'distance.km' is double
    elevation.gain.m = col_double(),         # Ensure 'elevation.gain.m' is double
    elapsed.time.hour = col_double(),        # Ensure 'elapsed.time.hour' is double
    moving.time.hour = col_double(),         # Ensure 'moving.time.hour' is double
    .default = col_character()               # Default to character for unspecified columns
  ))
}

# Function to process and upload the data for all years combined
process_all_data <- function(all_data, folder_id) {
  # Check if a combined TSV file already exists in the Google Drive folder
  existing_files <- drive_ls(as_id(folder_id), pattern = "Strava-activity-data\\.tsv")
  
  if (nrow(existing_files) > 0) {
    # If the file exists, download it
    drive_download(
      as_id(existing_files$id[1]),
      path = "Strava-activity-data.tsv",
      overwrite = TRUE
    )
    
    # Read the existing TSV into R with specified column types
    existing_data <- read_strava_data("Strava-activity-data.tsv")
    
    # Ensure consistent column types in existing_data
    existing_data <- existing_data %>%
      mutate(
        id = as.character(id),
        start_date = as.POSIXct(start_date, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
        average_heartrate = as.numeric(average_heartrate),
        max_heartrate = as.numeric(max_heartrate),
        start.date.local.day.of.year = as.numeric(start.date.local.day.of.year),
        start.year.local = as.numeric(start.year.local),
        start.dayofyear.local = as.numeric(start.dayofyear.local),
        start.month.local = as.character(start.month.local),
        start.week.local = as.numeric(start.week.local),
        distance.km = as.numeric(distance.km),
        elevation.gain.m = as.numeric(elevation.gain.m),
        elapsed.time.hour = as.numeric(elapsed.time.hour),
        moving.time.hour = as.numeric(moving.time.hour)
      )
    
    # Ensure consistent column types in all_data before combining
    all_data <- all_data %>%
      mutate(
        id = as.character(id),
        start_date = as.POSIXct(start_date, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
        average_heartrate = as.numeric(average_heartrate),
        max_heartrate = as.numeric(max_heartrate),
        start.date.local.day.of.year = as.numeric(start.date.local.day.of.year),
        start.year.local = as.numeric(start.year.local),
        start.dayofyear.local = as.numeric(start.dayofyear.local),
        start.month.local = as.character(start.month.local),
        start.week.local = as.numeric(start.week.local),
        distance.km = as.numeric(distance.km),
        elevation.gain.m = as.numeric(elevation.gain.m),
        elapsed.time.hour = as.numeric(elapsed.time.hour),
        moving.time.hour = as.numeric(moving.time.hour)
      )
    
    # Combine the new data with the existing data
    combined_data <- bind_rows(existing_data, all_data)
    
    # Write the combined data back to a single TSV file
    write_tsv(combined_data, "Strava-activity-data.tsv")
    
    # Upload the updated file back to Google Drive
    drive_upload(
      media = "Strava-activity-data.tsv",
      path = as_id(folder_id),
      name = "Strava-activity-data.tsv",
      overwrite = TRUE
    )
    
    cat("All data has been combined and the file has been updated.\n")
  } else {
    # If the file does not exist, create and upload it
    write_tsv(all_data, "Strava-activity-data.tsv")
    drive_upload(
      media = "Strava-activity-data.tsv",
      path = as_id(folder_id),
      name = "Strava-activity-data.tsv",
      overwrite = TRUE
    )
    cat("A new file has been created and uploaded to Google Drive.\n")
  }
}

# Assuming act_data.1 is your data containing Strava activities
# Ensure the data is in a suitable format
act_data_clean <- act_data.1 %>%
  mutate(
    id = as.character(id),
    start_date = as.POSIXct(start_date, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    average_heartrate = as.numeric(average_heartrate),
    max_heartrate = as.numeric(max_heartrate),
    start.date.local.day.of.year = as.numeric(start.date.local.day.of.year),
    start.year.local = as.numeric(start.year.local),
    start.dayofyear.local = as.numeric(start.dayofyear.local),
    start.month.local = as.character(start.month.local),
    start.week.local = as.numeric(start.week.local),
    distance.km = as.numeric(distance.km),
    elevation.gain.m = as.numeric(elevation.gain.m),
    elapsed.time.hour = as.numeric(elapsed.time.hour),
    moving.time.hour = as.numeric(moving.time.hour)
  )

# Process and upload all data as a single TSV file
process_all_data(act_data_clean, folder_id)

# Notify the user the export process is complete
cat("Export process complete.\n")
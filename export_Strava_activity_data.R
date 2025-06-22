#----------------------------------------------------------------------------------------------------------
# Program: C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_Strava-activity-data/export_Strava_activity_data.R
# Date created: 20-DEC-2024
# Author(s): Lun-Hsien Chang
# Modified from:
# Purposes: 
## Export strava activity data to 1 tsv file per year
## Export all strava activity data to 1 tsv file
## Date       Changes:
##---------------------------------------------------------------------------------------------------------
## 2025-06-21 Exported Strava activity data from Strava API to tsv files in Google Drive
## 2024-12-20 Moved customed functions from global.R, server.R to here
##---------------------------------------------------------------------------------------------------------

library(rStrava)
library(httr)
library(readr)
library(dplyr)
library(lubridate)
library(googledrive)

# === 1. Authenticate ===
stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])
googledrive::drive_auth()

# === 2. Google Drive folder ID ===
folder_id <- "1k495O3mJw56Vv5ldDI3C7yZK_JyAUB6A"

# === 3. Get all activities from Strava API ===
athlete_info <- rStrava::get_athlete(stoken, id = '37641772')
start_date <- as.Date(athlete_info$created_at)

my_acts <- rStrava::get_activity_list(
   stoken
  ,after = start_date
) 

act_data <- rStrava::compile_activities(my_acts) # dim(act_data) 1158 56

# Remove newline characters, such as \n and \r from these rows
##           id                                               name
## 1 7969844552 Holland Park west #1\nTed Smout Memorial Bridge #6
## 2 8014838770                   Dry land exercises #19, ODO321\n
## 3 8284066587   Holland Park west #3\nMorgan's Seafood Market #4
## 4 8806237812     Mt Coo-tha summit #93\nBrisbane Showgrounds #1

act_data_clean <- act_data %>%
  # Drop columns that are entirely NA
  dplyr::select(where(~ !all(is.na(.)))) %>%
  
  # Add activity year
  dplyr::mutate(activity_year = lubridate::year(as.Date(start_date))) %>%
  
  # Clean newline characters from all character columns
  dplyr::mutate(dplyr::across(
      dplyr::where(is.character)
     ,~ gsub("[\r\n]+", " ", .x) %>% trimws()
     )
    )

# Function to export data for a given year
export_strava_yearly_data <- function(act_data, year, folder_id, types_current) {
  if (!"activity_year" %in% names(act_data)) {
    act_data <- act_data %>%
      mutate(activity_year = lubridate::year(as.Date(start_date)))
  }
  
  yearly_data <- act_data %>%
    filter(activity_year == year) %>%
    select(-activity_year)
  
  if (nrow(yearly_data) == 0) {
    cat(sprintf("⚠️ No data found for year %d. Skipping.\n", year))
    return(invisible(NULL))
  }
  
  # Coerce types to match current_data
  for (col in names(types_current)) {
    if (col %in% names(yearly_data)) {
      target_type <- types_current[[col]]
      yearly_data[[col]] <- switch(target_type,
                                   "numeric" = suppressWarnings(as.numeric(yearly_data[[col]])),
                                   "character" = gsub("[\r\n]+", " ", as.character(yearly_data[[col]])) |> trimws(),
                                   "logical" = as.logical(yearly_data[[col]]),
                                   yearly_data[[col]]  # fallback: leave unchanged
      )
    }
  }
  
  file_name <- sprintf("Strava-activities_%d.tsv", year)
  
  readr::write_tsv(
    yearly_data,
    file = file_name
  )
  
  googledrive::drive_upload(
    media = file_name,
    path = googledrive::as_id(folder_id),
    name = file_name,
    overwrite = TRUE
  )
  
  cat(sprintf("✅ Exported %d records for year %d → %s\n", nrow(yearly_data), year, file_name))
}

# Export data to one tsv file per year. Only do this when current year is over
export_strava_yearly_data(act_data_clean, 2020, folder_id, types_current)
export_strava_yearly_data(act_data_clean, 2021, folder_id, types_current)
export_strava_yearly_data(act_data_clean, 2022, folder_id, types_current)
export_strava_yearly_data(act_data_clean, 2023, folder_id, types_current)
export_strava_yearly_data(act_data_clean, 2024, folder_id, types_current)

# Function to export full combined data
export_combined_strava_data <- function(act_data, folder_id) {
  combined_file <- "Strava-activity-data.tsv"
  
  # Strip helper column if present
  act_data_new <- act_data %>%
    select(-any_of("activity_year"))
  
  # Try to download existing file from Drive
  existing_files <- drive_ls(
    path = as_id(folder_id)
    ,pattern = "^Strava-activity-data\\.tsv$"
  )
  
  if (nrow(existing_files) > 0) {
    drive_download(
      file = as_id(existing_files$id[1])
      ,path = combined_file
      ,overwrite = TRUE
    )
    
    # Read existing file
    act_data_old <- read_tsv(combined_file, show_col_types = FALSE)
    
    # Combine and deduplicate by Strava activity ID
    full_data <- bind_rows(act_data_old, act_data_new) %>%
      distinct(id, .keep_all = TRUE)
    
    cat(sprintf("ℹ️ Existing file found: merged %d old + %d new → %d total\n",
                nrow(act_data_old), nrow(act_data_new), nrow(full_data)))
  } else {
    # No existing file, use only new data
    full_data <- act_data_new
    cat("ℹ️ No existing file found. Creating a new one.\n")
  }
  
  # Write merged data
  write_tsv(
    full_data
    ,combined_file
  )
  
  # Upload back to Drive
  drive_upload(
    media = combined_file
    ,path = as_id(folder_id)
    ,name = combined_file
    ,overwrite = TRUE
  )
  
  cat(sprintf("✅ Combined data (%d records) exported to %s\n", nrow(full_data), combined_file))
}

export_combined_strava_data(
   act_data=act_data_clean
  ,folder_id=folder_id)


cat("✅ All exports completed.\n")

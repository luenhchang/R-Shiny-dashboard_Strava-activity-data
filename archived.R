#----------------------------------------------------------------------------------------------------------
# Program: C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_Strava-activity-data/archived.R
# Modified from: C:/GoogleDrive_MyDrive/scripts/R-Shinyapp_Strava-activity-data/global.R
# Date created: 05-JULY-2024
# Author(s): Lun-Hsien Chang
# Input: 
# Output: 
# References
## Date       Changes:
##---------------------------------------------------------------------------------------------------------
## 2025-07-05 Changed source of historic data from Google Drive to local data folder
##---------------------------------------------------------------------------------------------------------

#-------------------------------------
# Read historic data from Google drive
#-------------------------------------
# Google Drive folder containing historical TSVs
folder_id <- "1k495O3mJw56Vv5ldDI3C7yZK_JyAUB6A"
years <- 2020:2024

# Enable non-interactive Google Drive access
googledrive::drive_auth(path = "python-dashboard-440410-ed98e19552ae.json")

# Read TSV from Drive directly using a temporary file
read_year_tsv <- function(year, types_current) {
  file_name <- sprintf("Strava-activities_%d.tsv", year)
  gd_file <- googledrive::drive_ls(googledrive::as_id(folder_id), pattern = file_name)
  
  if (nrow(gd_file) == 1) {
    temp_path <- tempfile(fileext = ".tsv")
    
    googledrive::drive_download(
      googledrive::as_id(gd_file$id),
      path = temp_path,
      overwrite = TRUE
    )
    
    col_spec <- make_col_spec_from_types(types_current)
    readr::read_tsv(temp_path, col_types = col_spec)
  } else {
    warning(sprintf("File not found for year %d in Google Drive.", year))
    return(NULL)
  }
}

# Read all historical data
historical_list <- lapply(years, read_year_tsv, types_current = types_current)
historical_data <- dplyr::bind_rows(Filter(Negate(is.null), historical_list)) # dim(historical_data) 954 51

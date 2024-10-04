#----------------------------------------------------------------------------------------------------------
# Program: C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_Strava-activity-data/chatGPT_code.R
# Date created: 04-OCT-2024
# Author(s): Lun-Hsien Chang, Modifications by ChatGPT
## Date       Changes:
##---------------------------------------------------------------------------------------------------------
##---------------------------------------------------------------------------------------------------------

# ChatGPT code:
years <- 2020:year(Sys.Date())
heatmaps <- list()  # Empty list to store plots

# Define the shared color scale and set zmin and zmax (common across heatmaps)
color_scale <- list(c(0, 1), c("lightgreen", "red"))

# Determine the range of elevation gain across all years
zmin <- min(ride.day$elevation.gain.m.day, na.rm = TRUE)
zmax <- max(ride.day$elevation.gain.m.day, na.rm = TRUE)

# Loop over the years and create heatmap for each year
for (year in years) {
  data_for_year <- ride.day %>% filter(start.year.local == year) # dim(data_for_year) 46 6
  
  # Check if data exists for the year
  if (nrow(data_for_year) == 0) {
    print(paste("No data for year:", year))
    next  # Skip this year if there's no data
  }
  
  # Print a small summary of the data for debugging
  print(paste("Creating plot for year:", year, "with", nrow(data_for_year), "rows"))
  
  # Create a heatmap by year and add it to the list
  name <- paste("plot", year, sep = "_")
  heatmaps[[name]] <- plotly_build(
    plot_ly(data = data_for_year,
            x = ~start.week.local,
            y = ~factor(start.day.local, 
                        levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
            z = ~elevation.gain.m.day,
            type = "heatmap",
            colors = colorRamp(c("lightgreen", "red")),
            zmin = zmin,   # Set shared zmin
            zmax = zmax,   # Set shared zmax
            colorbar = list(title = "Elevation per day", titleside = "right", len = 0.75),
            showscale = FALSE  # Disable individual color scales for each plot
    ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )
  )
}

# Create the combined plot with one shared color scale
final_plot <- subplot(heatmaps$plot_2022, heatmaps$plot_2023, heatmaps$plot_2024,
                      nrows = 1, shareX = FALSE, shareY = TRUE, margin = 0.05) %>% 
  layout(
    annotations = list(
      list(x = 0.1, y = 1, text = "2022", showarrow = FALSE, xref = 'paper', yref = 'paper'),
      list(x = 0.5, y = 1, text = "2023", showarrow = FALSE, xref = 'paper', yref = 'paper'),
      list(x = 0.9, y = 1, text = "2024", showarrow = FALSE, xref = 'paper', yref = 'paper')
    ),
    coloraxis = list(
      colorscale = colorRamp(c("lightgreen", "red")),  # Apply shared color scale
      colorbar = list(title = "Elevation per day", len = 0.75)  # Display a single color bar
    )
  )

# Show the plot
final_plot

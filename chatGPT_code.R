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

# Define a shared color scale (explicit color stops from 0 to 1)
color_scale <- list(
  list(0, "lightgreen"),  # Start of the scale
  list(1, "red")          # End of the scale
)

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
  name <- paste("plot",year,sep = "_")
  heatmaps[[name]] <- plotly_build(
    plot_ly(data = data_for_year
            ,x= ~start.week.local
            ,y= ~factor(start.day.local
                        ,levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
            )
            ,z= ~elevation.gain.m.day
            ,type = "heatmap"
            ,colorscale = color_scale  # Shared colorscale across heatmaps
            ,zmin = 0  # Minimum value for the colorscale
            ,zmax = 1000  # Maximum value for the colorscale (you can adjust based on your data)
            ,showscale = TRUE  # Show colorbar
            ,colorbar = list(
              len = 0.3
              ,y = 1
              ,yanchor = 'top'
              ,title = 'Elevation gain')
    ) %>%
      layout(xaxis = list(title = "")
             ,yaxis = list(title = ""
                           ,categoryorder = "array"
                           ,categoryarray = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
                           )
             ) # Close layout()
    ) # Close plotly_build()
}

# Create the subplot with shared colorscale
final_plot <- subplot(
  heatmaps$plot_2022, heatmaps$plot_2023, heatmaps$plot_2024,
  nrows = 1, shareX = FALSE, shareY = TRUE, margin = 0.05
) %>%
  layout(
    annotations = list(
      list(x = 0.1, y = 1, text = "2022", showarrow = FALSE, xref = 'paper', yref = 'paper'),
      list(x = 0.5, y = 1, text = "2023", showarrow = FALSE, xref = 'paper', yref = 'paper'),
      list(x = 0.9, y = 1, text = "2024", showarrow = FALSE, xref = 'paper', yref = 'paper')
      ) # Close list()
    ) # Close layout()

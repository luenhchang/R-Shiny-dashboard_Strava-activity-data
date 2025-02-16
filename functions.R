#----------------------------------------------------------------------------------------------------------
# Program: C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_Strava-activity-data/functions.R
# Date created: 19-DEC-2024
# Author(s): Lun-Hsien Chang
# Modified from:
# Reference
## [R plotly hover label text alignment](https://stackoverflow.com/questions/50003531/r-plotly-hover-label-text-alignment)
## [Multiple lines in plotly R not using add_trace](https://stackoverflow.com/questions/69567488/multiple-lines-in-plotly-r-not-using-add-trace)
## [Add border to stacked bar chart in plotly R](https://stackoverflow.com/questions/49868649/add-border-to-stacked-bar-chart-in-plotly-r)
## Date       Changes:
##---------------------------------------------------------------------------------------------------------
## 2025-02-16 Replaced plotly::plot_ly(text = ~gear_name_display, textposition = "inside") with plotly::add_annotations(x = 0.1, y = data$gear_name_factor[i], text = data$gear_name_display[i]) for better control of text placement
## 2025-02-15 Moved function.plotly.horizontal.bar.plot.gear.usage(data, metric_variable) from global.R here
## 2024-12-19 Moved customed functions from global.R, server.R to here
##---------------------------------------------------------------------------------------------------------

#-----------------------------
# Functions to use in global.R
#-----------------------------

# User-defined function for NA
not_all_na <- function(x) any(!is.na(x)) # if all values are NA
not_any_na <- function(x) all(!is.na(x)) # if any value is NA

#-----------------------------
# Functions to use in server.R
#-----------------------------
# Color
color.global.infoBox <- "olive"
color.global.valueBox <- "orange"

marker_style <- list(line = list(width = 0.5,
                                 color = 'rgb(0, 0, 0)'))

# Function to get the number of days in a given year
days_in_year <- function(year) {
  if (lubridate::leap_year(year)) {
    return(366)
  } else {
    return(365)
  }
}

# Get the current year
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# Function to check if a year is the current year
is_current_year <- function(year) {
  return(year == current_year)
}

# Example usage
#is_current_year(2024)  # Returns TRUE if the current year is 2024
#is_current_year(2023)  # Returns FALSE if the current year is not 2023

#------------------------------------------------------------------------------------------------
# Create functions for output
## [Create Function for output in Shiny](https://stackoverflow.com/questions/53590526/create-function-for-output-in-shiny)
## ["Correct" way to specifiy optional arguments in R functions](https://stackoverflow.com/questions/28370249/correct-way-to-specifiy-optional-arguments-in-r-functions)
## [How do you use "<<-" (scoping assignment) in R?](https://stackoverflow.com/questions/2628621/how-do-you-use-scoping-assignment-in-r)
#------------------------------------------------------------------------------------------------
# Define function for renderValueBox()
function.renderValueBox <- function(shiny_output
                                    ,output.id
                                    ,argument.value
                                    ,argument.subtitle
                                    ,argument.icon
                                    ,argument.icon.lib
                                    ,argument.color){
  # Write default values to optional arguments
  if(missing(argument.icon)){argument.icon <- "th-list"}
  if(missing(argument.icon.lib)){argument.icon.lib <- "glyphicon"}
  if(missing(argument.color)){argument.color<-"orange"}
  
  shiny_output[[output.id]] <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = argument.value
      ,subtitle = argument.subtitle
      #,icon = icon(argument.icon, lib = argument.icon.lib)
      ,color = argument.color)
  }) # Close renderValueBox()
} # Close function{}


# Define function for renderInfoBox()
## Default icon set to trophy
function.renderInfoBox <- function(shiny_output, output.id, arg.title, arg.value, arg.icon, arg.color, arg.fill){
  # Write default values to optional arguments
  if(missing(arg.icon)){arg.icon<-"trophy"}
  if(missing(arg.color)){arg.color<-"olive"}
  if(missing(arg.fill)){arg.fill <- TRUE}
  
  shiny_output[[output.id]] <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      title = arg.title
      ,value = arg.value
      ,icon=icon(arg.icon)
      ,color = arg.color
      ,fill = arg.fill)
  }) # Close renderInfoBox()
}# Close function{}

#----------------------------------------------------------------------------
# Function to create a horizontal bar plot on the usage of sport gear (shoes)
#----------------------------------------------------------------------------
## Required these variables in input data: gear_name_factor, metric variable (in quotes)
function.plotly.horizontal.bar.plot.gear.usage <- function(shiny_output
                                                           ,output.id
                                                           ,data
                                                           ,metric_variable) {
  shiny_output[[output.id]] <- plotly::renderPlotly({
    # Create the horizontal bar plot using sorted data
    fig <- plotly::plot_ly(
      data = data
      ,x = ~.data[[metric_variable]]
      ,y = ~ gear_name_factor # Maintain factor ordering
      ,type = 'bar'
      ,orientation = 'h'
      #,text = ~gear_name_display  # Display formatted labels
      ,textposition = "none"  # Hide default text placement #"inside"
      ,marker = list(color = 'lightblue')) %>%
      plotly::layout(
         xaxis = list(title = gsub("\\.", " ", metric_variable))
        ,yaxis = list(title=""
                      ,showticklabels = FALSE  # Hides y-axis tick labels
                      ,automargin = TRUE # adjusts spacing
                      ) 
        ,margin = list( l = 10 # Reduce left margin to minimize white space
                       ,r = 100 # Increase right margin to accommodate gear_name_display next to longest bars
                       )  
        ) # Close layout()
    
    # Add text annotations
    ## Iterate over each row of data and adds an annotation (text label) to the plot for each bar
    ### seq_len(nrow(data)) generates a sequence from 1 to the number of rows
    for (i in seq_len(nrow(data))) {
      fig <- fig %>%
        plotly::add_annotations(
           x = 0.1 # Align text to the left boundary of bars
          ,y = data$gear_name_factor[i]
          ,text = data$gear_name_display[i]
          ,showarrow = FALSE
          ,font = list(size = 12, color = "black")
          ,xanchor = "left" # Control the anchor point of the text relative to its x position
          ,align = "left" # Control the alignment of multi-line text within the annotation box.
          )
    }
    fig
  }) # Close renderPlotly()
}

#-------------------------------------------------------------------------------------------------------------------
# Program: C:/GoogleDrive/scripts/R-Shinyapp_Strava-activity-data/server.R
# Date created: 19-JUL-2024
# Author(s): Lun-Hsien Chang
# Modified from: C:/GoogleDrive/scripts/R-shinyapp_data-in-everyday-lives/server.R
# Dependency:
# Reference
## [R plotly hover label text alignment](https://stackoverflow.com/questions/50003531/r-plotly-hover-label-text-alignment)
## Date       Changes:
##--------------------------------------------------------------------------------------------------------------
## 2024-06-04 Added plotly bar plot subplots, one per food category. Currently no control on bar color and subplot titles
## 2024-05-16 Commented out shiny::renderUI({rmarkdown::render() shiny::includeHTML('menuItem-Fitness.html') }). Now includeHTML() is used to read a html file, which was knitted as html_fragment in .Rmd files
## 2024-05-07 Added a linebreak in renderDataTable with 2 steps: (1) add <br> as linebreak symbol, (2) set datatable(escape=FALSE)
## 2024-05-06 Used valueBox(subtitle=HTML(paste("<b>","text to bold","</b>")) to bold text in valueBox subtitle.
## 2024-05-06 Used valueBox(subtitle=HTML(paste(br()))) to add a new line in valueBox subtitle. 
## 2024-04-24 Created function.renderValueBox(), function.renderInfoBox() with optional arguments that have default values to generate script for renderValueBox(), renderInfoBox()
## 2024-04-23 Added a image slideshow under menuItem Data 
##--------------------------------------------------------------------------------------------------------------

# Color
color.global.infoBox <- "olive"
color.global.valueBox <- "orange"

server <- function(input, output, session) {
  
  # Stop running shiny app when closing browser window
  ## [How to stop running shiny app by closing the browser window?](https://stackoverflow.com/questions/35306295/how-to-stop-running-shiny-app-by-closing-the-browser-window)
  ## This is a bad idea! If multiple users are connected to the app, then one user (or browser tab) exiting will cause everyone to be kicked off! – Joe Cheng Aug 7, 2020 at 19:23
  session$onSessionEnded(function() { stopApp() })
  
  #------------------------------------------------------------------------------------------------
  # Create functions for output
  ## [Create Function for output in Shiny](https://stackoverflow.com/questions/53590526/create-function-for-output-in-shiny)
  ## ["Correct" way to specifiy optional arguments in R functions](https://stackoverflow.com/questions/28370249/correct-way-to-specifiy-optional-arguments-in-r-functions)
  ## [How do you use "<<-" (scoping assignment) in R?](https://stackoverflow.com/questions/2628621/how-do-you-use-scoping-assignment-in-r)
  #------------------------------------------------------------------------------------------------
  # Define function for renderValueBox()
  function.renderValueBox <- function(output.id
                                      ,argument.value
                                      ,argument.subtitle
                                      ,argument.icon
                                      ,argument.icon.lib
                                      ,argument.color){
    # Write default values to optional arguments
    if(missing(argument.icon)){argument.icon <- "th-list"}
    if(missing(argument.icon.lib)){argument.icon.lib <- "glyphicon"}
    if(missing(argument.color)){argument.color<-"orange"}
    
    output[[output.id]] <<- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = argument.value
        ,subtitle = argument.subtitle
        #,icon = icon(argument.icon, lib = argument.icon.lib)
        ,color = argument.color)
    }) # Close renderValueBox()
  } # Close function{}
  
  # Define function for renderInfoBox()
  function.renderInfoBox <- function(output.id, arg.title, arg.value, arg.icon, arg.color, arg.fill){
    # Write default values to optional arguments
    if(missing(arg.icon)){arg.icon<-"list"}
    if(missing(arg.color)){arg.color<-"olive"}
    if(missing(arg.fill)){arg.fill <- TRUE}

    output[[output.id]] <<- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        title = arg.title
        ,value = arg.value
        #,icon=icon(arg.icon)
        ,color = arg.color
        ,fill = arg.fill)
    }) # Close renderInfoBox()
  }# Close function{}
  
  #*****************************************
  # Outputs to use under menuItem "Swim"
  #*****************************************
  # Test plot when other plotly plots not working
  # output$plotly.test <- plotly::renderPlotly({
  #   plot_ly(x = 1:10, y = rnorm(10), type = 'scatter', mode = 'markers')
  # })
  
  output$plotly.bubble.plot.swim.pace.distance <- plotly::renderPlotly({
    # Make a bubble plot
    poolswim.combined |> 
    plotly::plot_ly(x=~Activity_date
                    ,y= ~pace_mmss_per100meters_fmt
                    ,text= ~hovertext #~Laps_training
                    ,hoverinfo = 'text'  # Display only the custom hover text
                    ,type = 'scatter'
                    ,size = ~ Corrected_distance_meters
                    ,mode= "markers"
                    ,marker = list(color="black"
                                   ,line=list(color='red'
                                              #,width=2
                                              )
                                   ,opacity = 0.9
                                   )
                    ) |>
      plotly::layout( xaxis=list(title="Date")
                      ,yaxis=list(title="Pace (mm::ss/100 meters)")
                      ,hoverlabel = list(align = "left") # Left-align hover text
                      ,annotations = list(
                         x = 0  # x-position of the annotation
                        ,y = 1    # y-position of the annotation
                        ,xref = 'paper'
                        ,yref = 'paper'
                        ,text = 'Bubble size represents distance (meters)'
                        ,showarrow = FALSE
                        ,font = list(size = 12)
                        ,align = "left"
                      )
      ) # Close layout
})
  
  #----------------
  # Output valueBox
  # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  #----------------
  # function.renderValueBox(output.id="valueBox.num.unique.food.barcodes"
  #                         ,argument.value=num.unique.food.barcode
  #                         ,argument.subtitle="Barcodes scanned")
  # 
  # function.renderValueBox(output.id="valueBox.num.food.category"
  #                         ,argument.value=num.food.category
  #                         ,argument.subtitle="Food categories")
  # 
  # function.renderValueBox(output.id = "valueBox.num.food.no.category"
  #                         ,argument.value = num.food.no.category
  #                         ,argument.subtitle = "Uncategorised foods"
  #                         ,argument.icon ="thumbs-down" )
  # 
  # function.renderValueBox(output.id="valueBox.num.food.expired"
  #                         ,argument.value=num.food.expired
  #                         ,argument.subtitle="Foods expired"
  #                         ,argument.icon ="thumbs-down" )
  # 
  # function.renderValueBox(output.id="valueBox.summed.price.food"
  #                         ,argument.value=paste0("$AUD ",summed.price.food)
  #                         ,argument.subtitle="Spent on food"
  #                         ,argument.icon ="credit-card")

  #----------------------------------------------------------------------------------------
  # Output DT dataTable
  ## Ref [How can I introduce a new line within a column using DTedit and shiny::uiOutput?](https://stackoverflow.com/questions/56590555/how-can-i-introduce-a-new-line-within-a-column-using-dtedit-and-shinyuioutput)
  #----------------------------------------------------------------------------------------
  output$table.poolswim <- DT::renderDataTable({
    table.poolswim <- poolswim.combined |> 
      dplyr::select(Strava_activity_name, Activity_date, Corrected_distance_meters, Corrected_moving_time, pace_mmss_per100meters_fmt, average_heartrate, max_heartrate) |>
      dplyr::rename(Event=Strava_activity_name
                    ,Date=Activity_date
                    ,`Distance(m)`=Corrected_distance_meters
                    ,Time=Corrected_moving_time
                    ,`Pace (hh:mm:ss/100 meters)`=pace_mmss_per100meters_fmt
                    ,`Avg heartrate`=average_heartrate
                    ,`Max heartrate`=max_heartrate) # dim(table.food.category.product.name) 122 2
    
    DT::datatable(table.poolswim
                  ,options = list(autoWidth = FALSE, searching = TRUE))
  })
  
  #*****************************************
  # Read data to use under menuItem "Walk" 
  #*****************************************
  output$plotly.bubble.plot.walk.pace.distance <- plotly::renderPlotly({
  # Make a bubble plot
  walk |> 
    plotly::plot_ly( x= ~start_date
                    ,y= ~ pace_mmss_per_km_fmt
                    ,text= ~hovertext #~Laps_training
                    ,hoverinfo = 'text'  # Display only the custom hover text
                    ,type = 'scatter'
                    ,size = ~ distance
                    ,mode= "markers"
                    ,marker = list(color="black"
                                   ,line=list(color='red')
                                   ,opacity = 0.9
                                   )
                    ) |>
    plotly::layout( xaxis=list(title="Date")
                    ,yaxis=list(title="Averaged Pace (mm::ss/km)")
                    ,hoverlabel = list(align = "left") # Left-align hover text
                    ,annotations = list(
                      x = 0  # x-position of the annotation
                      ,y = 1    # y-position of the annotation
                      ,xref = 'paper'
                      ,yref = 'paper'
                      ,text = 'Bubble size represents distance (km)'
                      ,showarrow = FALSE
                      ,font = list(size = 12)
                      ,align = "left"
                    )
    ) # Close layout
})
  #----------------------------------------------------------------------------------------
  # Output DT dataTable
  #----------------------------------------------------------------------------------------
  output$table.walk <- DT::renderDataTable({
    table.walk <- walk |> 
      dplyr::select(name, start_date, distance, moving_time_period,total_elevation_gain, pace_mmss_per_km_fmt) |>
      dplyr::rename(Event=name
                    ,Date=start_date
                    ,`Distance (km)`=distance
                    ,`Moving ime`=moving_time_period
                    ,`Elevation gain (m)`=total_elevation_gain
                    ,`Avg Pace (mm:ss/ km)`=pace_mmss_per_km_fmt) # dim(table.food.category.product.name) 122 2
    DT::datatable(table.walk
                  ,options = list(autoWidth = FALSE, searching = TRUE))
  })
  #*****************************************
  # Read data to use under menuItem "Run" 
  #*****************************************
  output$plotly.bubble.plot.run.pace.distance <- plotly::renderPlotly({
    # Make a bubble plot
      plotly::plot_ly( data = run
                       ,x= ~start_date
                       ,y= ~ pace_mmss_per_km_fmt
                       ,text= ~hovertext #~Laps_training
                       ,hoverinfo = 'text'  # Display only the custom hover text
                       ,type = 'scatter'
                       ,size = ~ distance
                       ,mode= "markers"
                       ,marker = list(color="black"
                                      ,line=list(color='red')
                                      ,opacity = 0.9
                       )
      ) |>
      plotly::layout( xaxis=list(title="Date")
                      ,yaxis=list(title="Averaged Pace (mm::ss/km)")
                      ,hoverlabel = list(align = "left") # Left-align hover text
                      ,annotations = list(
                        x = 0  # x-position of the annotation
                        ,y = 1    # y-position of the annotation
                        ,xref = 'paper'
                        ,yref = 'paper'
                        ,text = 'Bubble size represents distance (km)'
                        ,showarrow = FALSE
                        ,font = list(size = 12)
                        ,align = "left"
                      )
      ) # Close layout
  })
  
  #----------------------------------------------------------------------------------------
  # Output DT dataTable
  #----------------------------------------------------------------------------------------
  output$table.run <- DT::renderDataTable({
    table.run <- run |> 
      dplyr::select(name, start_date, distance, moving_time_period, total_elevation_gain, pace_mmss_per_km_fmt, average_heartrate, max_heartrate ) |>
      dplyr::rename(Event=name
                    ,Date=start_date
                    ,`Distance (km)`=distance
                    ,`Moving ime`=moving_time_period
                    ,`Elevation gain (m)`=total_elevation_gain
                    ,`Avg Pace (mm:ss/ km)`=pace_mmss_per_km_fmt
                    ,`Avg heart rate`=average_heartrate
                    ,`Max heart rate`=max_heartrate) # dim(table.run) 16 8
    DT::datatable(table.run
                  ,options = list(autoWidth = TRUE, searching = TRUE))
  })
  
  
  

  
} # Close the server function

#************************************************************************************************#
#---------------------------------This is the end of this file ----------------------------------#
#************************************************************************************************#
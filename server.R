#----------------------------------------------------------------------------------------------------------
# Program: C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_Strava-activity-data/server.R
# Date created: 02-OCT-2024
# Author(s): Lun-Hsien Chang
# Modified from: C:/GoogleDrive/scripts/R-Shinyapp_Strava-activity-data/server.R
# Reference
## [R plotly hover label text alignment](https://stackoverflow.com/questions/50003531/r-plotly-hover-label-text-alignment)
## [Multiple lines in plotly R not using add_trace](https://stackoverflow.com/questions/69567488/multiple-lines-in-plotly-r-not-using-add-trace)
## Date       Changes:
##---------------------------------------------------------------------------------------------------------
## 2024-06-04 Added plotly bar plot subplots, one per food category. Currently no control on bar color and subplot titles
## 2024-05-16 Commented out shiny::renderUI({rmarkdown::render() shiny::includeHTML('menuItem-Fitness.html') }). Now includeHTML() is used to read a html file, which was knitted as html_fragment in .Rmd files
## 2024-05-07 Added a linebreak in renderDataTable with 2 steps: (1) add <br> as linebreak symbol, (2) set datatable(escape=FALSE)
## 2024-05-06 Used valueBox(subtitle=HTML(paste("<b>","text to bold","</b>")) to bold text in valueBox subtitle.
## 2024-05-06 Used valueBox(subtitle=HTML(paste(br()))) to add a new line in valueBox subtitle. 
## 2024-04-24 Created function.renderValueBox(), function.renderInfoBox() with optional arguments that have default values to generate script for renderValueBox(), renderInfoBox()
## 2024-04-23 Added a image slideshow under menuItem Data 
##---------------------------------------------------------------------------------------------------------

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
  
  #**********************************************
  # Outputs to use under menuItem "Active Time"
  #**********************************************
  
  #----------------
  # 2023 valueBoxes
  # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  #----------------
  function.renderValueBox(output.id="valueBox.year.in.sport.2023"
                          ,argument.value=unique(activities.2023$start.year.local)
                          ,argument.subtitle="Year in sport")
  
  function.renderValueBox(output.id="valueBox.number.days.active.2023"
                          ,argument.value=length(unique(activities.2023$start.date.local)) # 275
                          ,argument.subtitle="Days active")
  
  function.renderValueBox(output.id="valueBox.total.moving.hours.2023"
                          ,argument.value=paste(round(sum(activities.2023$moving.time.hour, na.rm = TRUE), digits = 0), "hours") # 275
                          ,argument.subtitle="Total active time")
  
  function.renderValueBox(output.id="valueBox.total.cycling.distance.2023"
                          ,argument.value=paste(format(round(sum(activities.2023$distance.km, na.rm = TRUE), digits = 0), nsmall = 0, big.mark = ","), "km") # 275
                          ,argument.subtitle="Total cycling distance")
  
  function.renderValueBox(output.id="valueBox.total.cycling.elevation.2023"
                          ,argument.value=paste(format(round(sum(activities.2023$elevation.gain.m, na.rm = TRUE), digits = 0), nsmall = 0, big.mark = ","), "km") # 275
                          ,argument.subtitle="Total cycling elevation gain")
  
  #-------------------------------------
  # 2023 active hours daily using plotly
  #-------------------------------------
  output$plot.barplot.activity.moving.time.2023 <- shiny::renderPlot({
    
    data.barplot.moving.time <- activities.2023 %>%  
      dplyr::filter(!is.na(moving.time.hour) & activity.type !="EBikeRide") # dim(data.barplot.moving.time) 387 21
    
    # Legend
    plot.legend.ordered.barplot.moving.time <- sort(unique(data.barplot.moving.time$activity.type)) # length(plot.legend.ordered.barplot.moving.time) 9
    
    # Color. Use a palette with multiple distinct colors
    colors.barplot.moving.time <- as.vector(pals::glasbey(n=length(plot.legend.ordered.barplot.moving.time))) # length(colors.barplot.moving.time) 9
    
    # Pair legend item and color
    legend.colors.barplot.moving.time <- c( Badminton=colors.barplot.moving.time[1]
                                            ,`Bike Fitting`=colors.barplot.moving.time[2]
                                            ,Ride=colors.barplot.moving.time[3]
                                            ,Run=colors.barplot.moving.time[4]
                                            ,`Strength & Stability workout`=colors.barplot.moving.time[5]
                                            ,Swim=colors.barplot.moving.time[6]
                                            ,`Table Tennis`=colors.barplot.moving.time[7]
                                            ,Walk=colors.barplot.moving.time[8]
                                            ,Yoga=colors.barplot.moving.time[9])
    #-------------------------------------------------
    # 2023 active hours daily using ggplot2
    ## Backup plot in case plotly plot breaks
    #-------------------------------------------------
    barplot.daily.moving.time <- ggplot2::ggplot(data.barplot.moving.time
                                                 ,aes(x=start.date.local
                                                      ,y=moving.time.hour
                                                      ,fill = activity.type))+
      ggplot2::geom_bar(position = "stack", stat = "identity") + 
      ggplot2::scale_x_date( 
        limits = as.Date(c('2023-01-01','2023-12-31'))
        ,expand = c(0, 0) # expand = c(0,0) to remove margins
        ,date_breaks = "1 month"
        ,date_minor_breaks = "1 week"
        ,date_labels = "%b" # Abbreviated month name in current locale (Aug)
        # Add a secondary x axis showing week number
        # ,sec.axis = ggplot2::sec_axis(
        #   trans= ~ .
        #   ,breaks= c(seq.Date(as.Date("2023-01-01"), by="month", length.out = 12)
        #              ,"2023-12-25")
        #   ,labels= scales::date_format("%W"))
      )+
      ggplot2::scale_y_continuous(limits = c(0,8)
                                  ,breaks = seq(from=0, to=8, by=1)
                                  ,expand = c(0, 0) # expand = c(0,0) to remove margins
      )+
      ggplot2::labs(x = ""
                    ,y = "Moving time (h)"
                    ,fill="Activity type")+
      # Remove default theme background color
      ggplot2::theme_bw()+
      ggplot2::theme_minimal()+
      # Apply The Economist theme
      ggthemes::theme_economist_white()+ # theme color gray
      #ggthemes::theme_economist()+ # theme color pale green
      ggplot2::theme(
        # hjust controls horizontal justification and vjust controls vertical justification. Values between 0 and 1
        ## hjust=0 means left-justified, hjust=1 means right-justified
        legend.position = "top"
        ,legend.justification='left'
        ,legend.direction='horizontal'
        ,legend.text = element_text(size = 15)
        ,legend.box = "vertical"
        # Reduce gap between legends
        ,legend.spacing.y = unit(-0.25, "cm")
        ,axis.title = element_text(size = 20)
        ,axis.text = element_text(size = 15)
        ,panel.grid.minor = element_blank()
        ,panel.grid.major = element_line(color = "gray", linewidth = 0.5)) +
      # Change bar colors
      ## Use ggplot2::scale_color_manual() to modify aesthetics = "colour"
      ## Use ggplot2::scale_fill_manual() to modify aesthetics = "fill"
      ggplot2::scale_fill_manual(
        # Use palette from pals package. as.vector() needed to remove color name
        values=legend.colors.barplot.moving.time
        # Reorder legend items from most frequent to least frequent
        # legend text longer than the cutoff is wrapped to multiple lines
        ,limits=stringr::str_wrap(plot.legend.ordered.barplot.moving.time, width=20))+ 
      # Control legend for fill= aesthetic
      ## This has no effect. Legend items are still in two lines 
      ggplot2::guides(colour = guide_legend(nrow = 1))
    
    # Return value as default display
    return(barplot.daily.moving.time)
  }) # Close renderPlot()
  
  #----------------
  # 2024 valueBoxes
  # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  #----------------
  function.renderValueBox(output.id="valueBox.year.in.sport.2024"
                          ,argument.value=unique(activities.2024$start.year.local)
                          ,argument.subtitle="Year in sport")
  
  function.renderValueBox(output.id="valueBox.number.days.active.2024"
                          ,argument.value=length(unique(activities.2024$start.date.local)) # 275
                          ,argument.subtitle="Days active")
  
  function.renderValueBox(output.id="valueBox.total.moving.hours.2024"
                          ,argument.value=paste(round(sum(activities.2024$moving.time.hour, na.rm = TRUE), digits = 0), "hours") # 275
                          ,argument.subtitle="Total active time")
  
  function.renderValueBox(output.id="valueBox.total.cycling.distance.2024"
                          ,argument.value=paste(format(round(sum(activities.2024$distance.km, na.rm = TRUE), digits = 0), nsmall = 0, big.mark = ","), "km") # 275
                          ,argument.subtitle="Total cycling distance")
  
  function.renderValueBox(output.id="valueBox.total.cycling.elevation.2024"
                          ,argument.value=paste(format(round(sum(activities.2024$elevation.gain.m, na.rm = TRUE), digits = 0), nsmall = 0, big.mark = ","), "km") # 275
                          ,argument.subtitle="Total cycling elevation gain")
  
  #--------------------------------------
  # 2024 active hours daily using plotly
  #--------------------------------------
  # Plot moving time in 2024 using ggplot2 as an interactive plot
  output$plotly.stacked.barplot.activity.moving.time.2024 <- plotly::renderPlotly({
    data.moving.time.2024 %>% 
      plotly::plot_ly( x = ~start.date.local
                       ,y = ~moving.time.hour
                       ,type = 'bar'
                       ,name = ~activity.type
                       ,color = ~activity.type
                       ,hoverinfo="text"
                       ,hovertext=paste(
                         "Activity Name:",data.moving.time.2024$name
                         ,"<br> Date :", paste0(
                           format(data.moving.time.2024$start.date.local, format="%B %d")
                           ," at "
                           ,lubridate::hour(data.moving.time.2024$start.datetime.local)
                           ,":"
                           ,lubridate::minute(data.moving.time.2024$start.datetime.local)
                           ) # Close paste()
                         ,"<br> Active hours :", paste(format(round(data.moving.time.2024$moving.time.hour, digits = 2), nsmall=2),"h")
                         ,"<br> Distance:", paste(
                           format(round(data.moving.time.2024$distance.km, digits = 2), nsmall=2)
                           ,"km")
                         )
                       ) %>%
      plotly::layout( xaxis=list(title="Date",titlefont= list(size=40),tickformat="%b")
                     ,yaxis = list(title = 'Hours',titlefont= list(size=40))
                     ,barmode = 'stack'
                     # Left align hover text
                     ,hoverlabel = list(align = "left"))
  })
  
  # Plot moving time in 2024 using ggplot2 as a static plot
  output$plot.barplot.activity.moving.time.2024 <- shiny::renderPlot({
    
    data.barplot.moving.time <- activities.2024 %>%  
      dplyr::filter(!is.na(moving.time.hour) & activity.type !="EBikeRide") # dim(data.barplot.moving.time) 206 21
    
    # Legend
    plot.legend.ordered.barplot.moving.time <- sort(unique(data.barplot.moving.time$activity.type)) # length(plot.legend.ordered.barplot.moving.time) 5
    
    # Color. Use a palette with multiple distinct colors
    colors.barplot.moving.time <- as.vector(pals::glasbey(n=length(plot.legend.ordered.barplot.moving.time))) # length(colors.barplot.moving.time) 5
    
    # Pair legend item and color
    legend.colors.barplot.moving.time <- c( Ride=colors.barplot.moving.time[1]
                                            ,Run=colors.barplot.moving.time[2]
                                            ,Swim=colors.barplot.moving.time[3]
                                            ,Walk=colors.barplot.moving.time[4]
                                            ,Workout=colors.barplot.moving.time[5])
    # Create a bar plot
    barplot.daily.moving.time <- ggplot2::ggplot(data.barplot.moving.time
                                                 ,aes(x=start.date.local
                                                      ,y=moving.time.hour
                                                      ,fill = activity.type))+
      ggplot2::geom_bar(position = "stack", stat = "identity") + 
      ggplot2::scale_x_date( 
        limits = as.Date(c('2024-01-01','2024-12-31'))
        ,expand = c(0, 0) # expand = c(0,0) to remove margins
        ,date_breaks = "1 month"
        ,date_minor_breaks = "1 week"
        ,date_labels = "%b" # Abbreviated month name in current locale (Aug)
        # Add a secondary x axis showing week number
        # ,sec.axis = ggplot2::sec_axis(
        #   trans= ~ .
        #   ,breaks= c(seq.Date(as.Date("2023-01-01"), by="month", length.out = 12)
        #              ,"2023-12-25")
        #   ,labels= scales::date_format("%W"))
      )+
      ggplot2::scale_y_continuous(limits = c(0,8)
                                  ,breaks = seq(from=0, to=8, by=1)
                                  ,expand = c(0, 0) # expand = c(0,0) to remove margins
      )+
      ggplot2::labs(x = ""
                    ,y = "Moving time (h)"
                    ,fill="Activity type")+
      # Remove default theme background color
      ggplot2::theme_bw()+
      ggplot2::theme_minimal()+
      # Apply The Economist theme
      ggthemes::theme_economist_white()+ # theme color gray
      #ggthemes::theme_economist()+ # theme color pale green
      ggplot2::theme(
        # hjust controls horizontal justification and vjust controls vertical justification. Values between 0 and 1
        ## hjust=0 means left-justified, hjust=1 means right-justified
        legend.position = "top"
        ,legend.justification='left'
        ,legend.direction='horizontal'
        ,legend.text = element_text(size = 15)
        ,legend.box = "vertical"
        # Reduce gap between legends
        ,legend.spacing.y = unit(-0.25, "cm")
        ,axis.title = element_text(size = 20)
        ,axis.text = element_text(size = 15)
        ,panel.grid.minor = element_blank()
        ,panel.grid.major = element_line(color = "gray", linewidth = 0.5)) +
      # Change bar colors
      ## Use ggplot2::scale_color_manual() to modify aesthetics = "colour"
      ## Use ggplot2::scale_fill_manual() to modify aesthetics = "fill"
      ggplot2::scale_fill_manual(
        # Use palette from pals package. as.vector() needed to remove color name
        values=legend.colors.barplot.moving.time
        # Reorder legend items from most frequent to least frequent
        # legend text longer than the cutoff is wrapped to multiple lines
        ,limits=stringr::str_wrap(plot.legend.ordered.barplot.moving.time, width=20))+ 
      # Control legend for fill= aesthetic
      ## This has no effect. Legend items are still in two lines 
      ggplot2::guides(colour = guide_legend(nrow = 1))
    
    # Return value as default display
    return(barplot.daily.moving.time)
  }) # Close renderPlot()
  
  
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
  
  #--------------------
  # Output DT dataTable
  #--------------------
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
  
  #*****************************************
  # Read data to use under menuItem "Ride" 
  #*****************************************
  #----------------------------------------------------------------------
  # Yearly cycling elevation gain per weekday by plotly calendar heatmaps
  #----------------------------------------------------------------------
  output$plotly.calendar.heatmap.yearly.ride.elevation.day <- plotly::renderPlotly({
    years <- 2020:year(Sys.Date())
    heatmaps <- list()  # Empty list to store plots
    
    # Define a purple to yellow color scale
    color_scale <- list(
      list(0, "purple"),  # Start of the scale
      list(0.5, "orange"), # Midpoint of the scale
      list(1, "yellow")   # End of the scale
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
                  len = 1
                  ,y = 1
                  ,yanchor = 'top'
                  ,title = 'Elevation gain'
                  ,tickvals = seq(0, 1000, by = 250)  # Set color scale ticks to 0, 100, 200, ..., 1000
                  ,ticktext = seq(0, 1000, by = 250)  # Optional: Specify tick labels for colorbar
                  )
        ) %>%
          layout( xaxis = list(title = "Week", dtick=10) # This has no effect changing title and tick interval
                 ,yaxis = list(title = ""
                               ,categoryorder = "array"
                               ,categoryarray = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
                 )
          ) # Close layout()
      ) # Close plotly_build()
    } # End for loop
    
    # Create the subplot with shared colorscale
    ## heatmaps$plot_2020, heatmaps$plot_2021 are blank probably because of missing data
    plotly.calendar.heatmaps.ride.elevation.day.yearly <- plotly::subplot(
       heatmaps$plot_2022
      ,heatmaps$plot_2023
      ,heatmaps$plot_2024
      ,nrows = 1
      ,shareX = FALSE
      ,shareY = TRUE
      ,margin = 0.05) %>%
      plotly::layout(annotations = list(
        list(x = 0.1, y = 1.075, text = "2022", font = list(size = 20), showarrow = FALSE, xref = 'paper', yref = 'paper')
        ,list(x = 0.5, y = 1.075, text = "2023", font = list(size = 20), showarrow = FALSE, xref = 'paper', yref = 'paper')
        ,list(x = 0.9, y = 1.075, text = "2024", font = list(size = 20), showarrow = FALSE, xref = 'paper', yref = 'paper')
        ) # Close list()
      ) %>%
      # Tweak first and second x axes to get a centre title 
      layout(
         xaxis = list(title="", titlefont= list(size=20), showgrid=FALSE)
        ,xaxis2 = list(title="Week", titlefont= list(size=20))
        )
    
    # Return the final plot
    plotly.calendar.heatmaps.ride.elevation.day.yearly
    
    }) # End renderPlotly()
  
  #----------------------------------------------------------------------
  # Yearly cycling distance per weekday by plotly calendar heatmaps
  #----------------------------------------------------------------------
  output$plotly.calendar.heatmap.yearly.ride.distance.day <- plotly::renderPlotly({
    years <- 2020:year(Sys.Date())
    heatmaps.ride.distance <- list()  # Empty list to store plots
    
    # Define a color scale
    color_scale <- viridis::viridis(256)  # Generates a color palette from dark blue to yellow
    
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
      heatmaps.ride.distance[[name]] <- plotly_build(
        plot_ly(data = data_for_year
                ,x= ~start.week.local
                ,y= ~factor(start.day.local
                            ,levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
                )
                ,z= ~distance.km.day
                ,type = "heatmap"
                ,colorscale = color_scale  # Shared colorscale across heatmaps
                ,zmin = 0  # Minimum value for the colorscale
                ,zmax = 200  # Maximum value for the colorscale (you can adjust based on your data)
                ,showscale = TRUE  # Show colorbar
                ,colorbar = list(
                  len = 1
                  ,y = 1
                  ,yanchor = 'top'
                  ,title = 'Distance'
                  ,tickvals = seq(0, 200, by = 50)  # Set color scale ticks to 0, 100, 200, ..., 1000
                  ,ticktext = seq(0, 200, by = 50)  # Optional: Specify tick labels for colorbar
                )
        ) %>%
          layout( xaxis = list(title = "Week", dtick=10) # This has no effect changing title and tick interval
                  ,yaxis = list(title = ""
                                ,categoryorder = "array"
                                ,categoryarray = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
                  )
          ) # Close layout()
      ) # Close plotly_build()
    } # End for loop
    
    # Create the subplot with shared colorscale
    ## heatmaps.ride.distance$plot_2020, heatmaps.ride.distance$plot_2021 are blank probably because of missing data
    plotly.calendar.heatmaps.ride.distance.day.yearly <- plotly::subplot(
      heatmaps.ride.distance$plot_2022
      ,heatmaps.ride.distance$plot_2023
      ,heatmaps.ride.distance$plot_2024
      ,nrows = 1
      ,shareX = FALSE
      ,shareY = TRUE
      ,margin = 0.05) %>%
      plotly::layout(annotations = list(
        list(x = 0.1, y = 1.075, text = "2022", font = list(size = 20), showarrow = FALSE, xref = 'paper', yref = 'paper')
        ,list(x = 0.5, y = 1.075, text = "2023", font = list(size = 20), showarrow = FALSE, xref = 'paper', yref = 'paper')
        ,list(x = 0.9, y = 1.075, text = "2024", font = list(size = 20), showarrow = FALSE, xref = 'paper', yref = 'paper')
      ) # Close list()
      ) %>%
      # Tweak first and second x axes to get a centre title 
      layout(
        xaxis = list(title="", titlefont= list(size=20), showgrid=FALSE)
        ,xaxis2 = list(title="Week", titlefont= list(size=20))
      )
    
    # Return the final plot
    plotly.calendar.heatmaps.ride.distance.day.yearly
  }) # End renderPlotly()
  
  #-------------------------------------------------------
  # Plot cumulative cycling distance over start.date.local
  #-------------------------------------------------------
  output$plotly.lineplot.yearly.ride.cumulative.distance <- plotly::renderPlotly({
    # Create line colors
    number.years <-length(unique(ride.day$start.year.local))
    colors.viridis <- viridis::viridis(number.years)
    
    lineplot.yearly.ride.cumulative.distance <- plotly::plot_ly(
       data = ride.day
      ,type = "scatter"
      ,mode = "lines+markers"
      ,x = ~start.dayofyear.local
      ,y = ~ride.distance.cum.year
      ,color = ~factor(start.year.local) # This maps color to each year
      ,colors = colors.viridis # Use viridis palette
      ) %>%
      layout(xaxis = list(title = "Day of year", dtick=30)
             ,yaxis = list(title = "", tickformat=",")
             )
    # Return the final plot
    lineplot.yearly.ride.cumulative.distance
  })
  
  #-------------------------------------------------------
  # Plot cumulative cycling elevation gain over start.date.local
  #-------------------------------------------------------
  output$plotly.lineplot.yearly.ride.cumulative.elevation <- plotly::renderPlotly({
    # Create line colors
    number.years <-length(unique(ride.day$start.year.local))
    colors.viridis <- viridis::viridis(number.years)
    # Display colors
    scales::show_col(colors.viridis)

    lineplot.yearly.ride.cumulative.elevation <- plotly::plot_ly(
      data = ride.day
      ,type = "scatter"
      ,mode = "lines+markers"
      ,x = ~start.dayofyear.local
      ,y = ~ride.elevation.cum.year
      ,color = ~factor(start.year.local) # This maps color to each year
      ,colors = colors.viridis # Use viridis palette
      ) %>%
      layout(xaxis = list(title = "Day of year", dtick=30)
             ,yaxis = list(title = "", tickformat=",")
      )
    # Return the final plot
    lineplot.yearly.ride.cumulative.elevation
  })
  
} # Close the server function

#************************************************************************************************#
#---------------------------------This is the end of this file ----------------------------------#
#************************************************************************************************#
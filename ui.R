#----------------------------------------------------------------------------------------------------------
# Program: C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_Strava-activity-data/ui.R
# Date created: 02-OCT-2024
# Author(s): Lun-Hsien Chang
# Modified from: C:/GoogleDrive/scripts/R-Shinyapp_Strava-activity-data/ui.R
# Dependency:
# Input:
## menuItem-About.html
# Output: https://luenhchang.shinyapps.io/barcode-scan/
# References
## [Horizontal Rule hr() in R Shiny Sidebar](https://stackoverflow.com/questions/43592163/horizontal-rule-hr-in-r-shiny-sidebar)
## Date       Changes:
##---------------------------------------------------------------------------------------------------------
## 2024-07-23 Added menuItem Walk
## 2024-07-21 Added menuItem Swim
##---------------------------------------------------------------------------------------------------------

#---------------------------------
# Webpage title on top left corner
#---------------------------------
application.title <- "Strava data"
application.title <- tags$a(href="#top" # A destination to go to. Can be a URL or locations in this app 
                            ,icon('chart-line')
                            ,"Strava data analytics")

#--------------------------------------------
# Define dashboardPage() function components
## Reference [https://stackoverflow.com/questions/67237358/set-font-size-and-color-in-title-of-shinydashboard-box](https://stackoverflow.com/questions/67237358/set-font-size-and-color-in-title-of-shinydashboard-box)
## [R-Shiny-Dashboards/USArrestDashboard/ui.R](https://github.com/aagarw30/R-Shiny-Dashboards/blob/main/USArrestDashboard/ui.R)
#--------------------------------------------
header <- shinydashboard::dashboardHeader(
  title = application.title
  ,titleWidth = 650
  ,tags$li(class="dropdown"
           ,tags$a(href="https://www.linkedin.com/in/lunhsienchang/", icon("linkedin", "My profile", target="_blank")))
  )

sidebar <- shinydashboard::dashboardSidebar(
  width = 200
  ,shinydashboard::sidebarMenu(
    # Change font size to 30
    ## Reference [shinydashboard: change font size of menuItem in sidebarMenu [duplicate]](https://stackoverflow.com/questions/53559195/shinydashboard-change-font-size-of-menuitem-in-sidebarmenu)
    tags$style(HTML(".sidebar-menu li a { font-size: 20px; }"))
    #,shinydashboard::menuItem(text = "About", tabName = "tabAbout",icon = icon("home",lib = "glyphicon"))
    ,shinydashboard::menuItem(text = "Swim", tabName = "tabSwim", icon = icon("chart-line"))
    ,shinydashboard::menuItem(text = "Walk", tabName = "tabWalk", icon = icon("chart-line"))
    ,shinydashboard::menuItem(text = "Run", tabName = "tabRun", icon = icon("chart-line"))
  ) # Close sidebarMenu()
)

body <- shinydashboard::dashboardBody(
  shinydashboard::tabItems(
    #************************************
    # menuItem "Swim"
    #************************************
    shinydashboard::tabItem(
      tabName = "tabSwim"
      # ,fluidRow(
      #   # valueBox 1 to 5
      #    shinydashboard::valueBoxOutput(outputId = "valueBox.num.unique.food.barcodes", width = 2)
      #   ,shinydashboard::valueBoxOutput(outputId = "valueBox.num.food.category", width = 2)
      #   ,shinydashboard::valueBoxOutput(outputId = "valueBox.num.food.no.category", width = 2)
      #   ,shinydashboard::valueBoxOutput(outputId = "valueBox.num.food.expired", width = 2)
      #   ,shinydashboard::valueBoxOutput(outputId = "valueBox.summed.price.food", width = 3)
      #   ) # Close fluidRow
      ,fluidRow(
        box(title="Pool swimming pace over distance"
            ,status = "primary"
            ,solidHeader = TRUE
            ,width = 12
            ,plotlyOutput(outputId = "plotly.bubble.plot.swim.pace.distance"
                        ))
      )
      ,fluidRow(
        box(title = "Pool swimming data"
             ,status = "primary"
             ,solidHeader = TRUE
             ,collapsible = TRUE
             ,width = 12
             ,dataTableOutput(outputId="table.poolswim"))
        ) # Close fluidRow
    ) # Close tabItem
    #************************************
    # menuItem "Walk"
    #************************************
    ,shinydashboard::tabItem(
      tabName = "tabWalk"
      ,fluidRow(
        box(title="Walking pace over distance"
            ,status = "primary"
            ,solidHeader = TRUE
            ,width = 12
            ,plotlyOutput(outputId = "plotly.bubble.plot.walk.pace.distance"
            ))
      )
      ,fluidRow(
        box(title = "Walking data"
             ,status = "primary"
             ,solidHeader = TRUE
             ,collapsible = TRUE
             ,width = 12
             ,dataTableOutput(outputId="table.walk"))
      ) # Close fluidRow
    ) # Close tabItem
    #************************************
    # menuItem "Run"
    #************************************
    ,shinydashboard::tabItem(
      tabName = "tabRun"
      ,fluidRow(
        box(title="Running pace over distance"
            ,status = "primary"
            ,solidHeader = TRUE
            ,width = 12
            ,plotlyOutput(outputId = "plotly.bubble.plot.run.pace.distance"
            ))
      )
      ,fluidRow(
        box(title = "Running data"
             ,status = "primary"
             ,solidHeader = TRUE
             ,collapsible = TRUE
             ,width = 12
             ,dataTableOutput(outputId="table.run"))
      ) # Close fluidRow
    ) # Close tabItem
    
  ) # Close tabItems
) # Close dashboardBody()

#-------------------------------------------------------------------------------------
# User interface by shinydashboard
## The dashboardPage() function expects three components: a header, sidebar, and body:
## References [shinydashboard](https://rstudio.github.io/shinydashboard/index.html)
#-------------------------------------------------------------------------------------
ui <- shinydashboard::dashboardPage(
  title = "Strava analytics" # A title to display in the browser's title bar. If no value is provided, it will try to extract the title from the dashboardHeader.
  #title = span(tagList(icon("calendar")),"Everyday Data")
  ,header=header
  ,sidebar=sidebar
  ,body=body
  ,skin = "black")

#************************************************************************************************#
#---------------------------------This is the end of this file ----------------------------------#
#************************************************************************************************#
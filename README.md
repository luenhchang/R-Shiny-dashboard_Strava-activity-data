# R Shiny Dashboard: Strava Activity Data

## Latest Updates
- **2024-12-19**: 
* Moved custom functions from `global.R` and `server.R` to `functions.R`
* Sorted all DT dataTables by descending orders of Date and Activity
* Updated Plotly bar plot to show every month on the X axis
* Changed `Moving time` from lubridate to character to keep 'xxH xxM xxS' format in DT dataTables

---

## Latest Updates
- Last updated: <!--LAST_UPDATED-->

---

## Introduction
This repository contains an interactive R Shiny application that connects to the Strava API to retrieve and visualise activity data from a single user. The app provides insights into activity trends, performance metrics, and much more in an engaging and user-friendly interface.

---

## Features
- **Data Retrieval**: Fetches activity data directly from the Strava API and Google sheet
- **Visualisations**: Includes interactive plots, static plots and metrics for activity summaries, trends, and comparisons.
- **Interactivity**: Offers hovertext, tables with search and sorting features for detailed analysis.

---

## Demo
You can view the live app here: [Strava activity data](https://luenhchang.shinyapps.io/Strava-activity-data/).

---

## Screenshots

### Activity Trends Dashboard
![Active hours 2024](app-printscreens/moving-time-2024.png)

---

![Active hours 2023](app-printscreens/moving-time-2023.png)

---

![Ride data](app-printscreens/Ride-2023-2024.png)

---

### Tables
![Active hours](app-printscreens/table_active-hours.png)

---

![Pool swimming data](app-printscreens/table_pool-swimming-data.png)

---

## Installation
To run this application locally:
1. Clone this repository:
   ```bash
   git clone https://github.com/luenhchang/R-Shiny-dashboard_Strava-activity-data.git
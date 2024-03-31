library(shiny)
library(leaflet)
library(shinythemes)
library(dplyr)
library(sf)

# Load the shapefile using relative path
plotts <- st_read("data/fpc9.shp")

print(str(plotts))


# Rename the column names of plotts to desired names


# Rename variables in plotts data frame
plotts <- plotts %>%
  rename(
    COUNTY = COUNTY_y,
    `Women Indv Samp Weight` = v005,
    `Predicted vaccination Medians` = f,
    `Respondents current age` = v012,
    `Pri Sampling Unit` = v021,
    `Sample Strata for Errors` = v022,
    `Stratification in sample design` = v023,
    Region = v024,
    `Type of place of Residence` = v025,
    `Cluster altitude in meters` = v040,
    `Highest Education level` = v106,
    Electricity = v119,
    Transport = TRANSPORT,
    Religion = v130,
    Ethnicity = v131,
    Media = MEDIA,
    `Wealth index combined` = v190,
    `Marital Status` = v501,
    `Year of Birth` = b2,
    `sex of child` = b4,
    `Has Health card/ vaccination document` = h1a
  )


print(str(plotts))

# Define UI
ui <- fluidPage(
  # Application title with padding
  titlePanel(
    div(
      style = "font-weight: bold; font-family: 'Arial', sans-serif; color: #333333; font-size: 24px; padding-bottom: 20px;",
      "GWRF Model Local Variable Importance"
    ),
    windowTitle = "GWRF Model Importance"
  ),
  
  # Sidebar layout with increased height
  sidebarLayout(
    sidebarPanel(
      # Dropdown for attribute selection
      selectInput("attribute", "Select Attribute:", choices = colnames(subset(plotts, select = -c(COUNTY))), width = "100%"),
      # Help text
      tags$hr(),
      helpText(
        "Select an attribute from the dropdown to visualize its importance on the map.",
        style = "font-size: 14px; color: #555555;"
      ),
      tags$hr()
    ),
    
    # Main panel for map display
    mainPanel(
      leafletOutput("map", width = "100%", height = "800px")  # Increased height for the map display
    )
  ),
  
  
  
  # Apply dark theme
  theme = shinytheme("darkly"),
  
  # Custom CSS for background color and padding
  tags$style(
    HTML(
      "
      body {
        background-color: #F0F0F0; /* Light gray background */
        padding: 20px; /* Add padding to the body */
      }
      "
    )
  )

)

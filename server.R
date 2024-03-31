library(leaflet)
library(sf)
library(dplyr)

# Load the shapefile or data
shapefile_path <- "data/fpc9.shp"
plotts <- st_read(shapefile_path)

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

# Define server logic
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    pal <- colorNumeric(
      palette = "Spectral",
      domain = plotts[[input$attribute]]
    )
    
    leaflet(plotts) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = 37.5, lat = 0, zoom = 5) %>%
      addPolygons(
        fillColor = ~pal(plotts[[input$attribute]]),
        fillOpacity = 0.7,
        color = "#BDBDC3",
        weight = 1,
        stroke = TRUE,
        highlight = highlightOptions(
          weight = 2,
          color = "black",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste(plotts$COUNTY, ": ", plotts[[input$attribute]], sep = ""),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~plotts[[input$attribute]],
        title = input$attribute,
        opacity = 0.7,
        position = "bottomright"
      )
  })
}

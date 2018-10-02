
# This file is part of ITHIM Sacramento.

# File: App.R
# Purpose: Reading necessary functions for deploying the shiny.app. And shiny app implementation

# https://www.computerworld.com/article/3038270/data-analytics/create-maps-in-r-in-10-fairly-easy-steps.html?page=3

# library definitions
library(shiny)
library(leaflet)

###################### ITHIM application for Equity Analysis - Web Interface - Shiny App - Server/UI ######################
# app.R has 2 main components, UI and Server.
# User Interface (UI)
#     -Map
# Server Function
#     -Leaflet Map
# App Function

# User Interface ============================================================================================================
# Uses fluidPage and navbar for Layout
# Reading geojson
CA_HO<-st_read("SACOG_HO_01_02.geojson")
CA_HO <- sf::st_transform(CA_HO, "+proj=longlat +datum=WGS84") 

# Creating color palatte
FutureMin <- min(c(CA_HO$Abs_death_Comb_2020, CA_HO$Abs_death_Comb_2027, CA_HO$Abs_death_Comb_2036))
FutureMax <- max(c(CA_HO$Abs_death_Comb_2020, CA_HO$Abs_death_Comb_2027, CA_HO$Abs_death_Comb_2036))

ScenarioMin <- min(c(CA_HO$Abs_death_Comb_S1, CA_HO$Abs_death_Comb_S2, CA_HO$Abs_death_TI_S3))
ScenarioMax <- max(c(CA_HO$Abs_death_Comb_S1, CA_HO$Abs_death_Comb_S2, CA_HO$Abs_death_Comb_S3))

palfuture <- colorNumeric("BrBG" ,domain = c((FutureMax*-1),FutureMax))
palScenario <-colorNumeric("PuOr" ,-15:15)

# Creating pop-up
SacHOpopup <- paste0("Zip Code: ", CA_HO$ZCTA5CE10, "<br>"
                     ,"Absolute Change in Deaths 2020: ", CA_HO$Abs_death_Comb_2020, "<br>",
                     "Absolute Change in Deaths 2027: ", CA_HO$Abs_death_Comb_2027, "<br>",
                     "Absolute Change in Deaths 2036: ", CA_HO$Abs_death_Comb_2036
)
SacHOpopup_Scenarios <- paste0("Zip Code: ", CA_HO$ZCTA5CE10 , "<br>"
                               ,"Absolute Change in Deaths Scenario 1: ", CA_HO$Abs_death_Comb_S1, "<br>",
                               "Absolute Change in Deaths Scenario 2: ", CA_HO$Abs_death_Comb_S2, "<br>",
                               "Absolute Change in Deaths Scenario 3: ", CA_HO$Abs_death_Comb_S3
)

ui <- tabsetPanel(
  #Creates Title Tab
  # titlePanel("ITHIM-Sacramento Spatial Analysis Tool"),

  # navbarPage(
  # "ITHIM-SACOG",
          
# Creates Future Map Tab
    # Creates About Page
    tabPanel("About",
             includeHTML("ITHIM_Spatial_About.html")# Pulls About page from Markdown File
    ),
  tabPanel("2016 MTP/SCS Adopted Plan in Future Years",
           leaflet(CA_HO) %>%
             setView(-121, 39, 8) %>%
             addProviderTiles("CartoDB.Positron")%>%
             #Set up Legend for Future
             addLegend("topright",
                       pal = palfuture,
                       values = ~c(FutureMin,FutureMax),
                       title = "2016 MTP/SCS Adopted Plan for Future Years <br>
              Absolute Change in Deaths",
                       labFormat = labelFormat(digits = 3),
                       opacity = 1
             )%>%
             #Add Layers
             addPolygons(data = CA_HO,
                         popup=SacHOpopup,
                         fillColor = ~palfuture(Abs_death_Comb_2020),
                         fillOpacity = 0.8,
                         color = "white",
                         weight = 1,
                         group = "2020"
             )%>%
             addPolygons(data = CA_HO,
                         popup=SacHOpopup,
                         fillColor = ~palfuture(Abs_death_Comb_2027),
                         fillOpacity = 0.8,
                         color = "white",
                         weight = 1,
                         group = "2027"
             )%>%
             addPolygons(data = CA_HO,
                         popup=SacHOpopup,
                         fillColor = ~palfuture(Abs_death_Comb_2036),
                         fillOpacity = 0.8,
                         color = "white",
                         weight = 1,
                         group = "2036"
             )%>%
             #Add Layers Radio Buttons
             addLayersControl(
               baseGroups= c("2020", "2027", "2036"),
               position = "bottomleft",
               options = layersControlOptions(collapsed = FALSE)
             )
          ),
  tabPanel( "Planning Scenarios in 2036",
    leaflet(CA_HO) %>%
      setView(-121, 39, 8) %>%
      addProviderTiles("CartoDB.Positron")%>%
      addPolygons(data = CA_HO,
                  popup=SacHOpopup,
                  fillColor = ~palfuture(Abs_death_Comb_2020),
                  fillOpacity = 0.8,
                  color = "white",
                  weight = 1,
                  group = "2020"
                  )%>%
      #Set up Legend for Scenarios
      addLegend("bottomright", 
                pal = palScenario, 
                values = ~c(-15,15),
                title = "Planning Scenarios in 2036 <br>
                Absolute Change in Deaths",
                labFormat = labelFormat(digits = 3),
                opacity = 1
      )%>%
      # Add Scenario Layers
      addPolygons(data = CA_HO,
                  popup=SacHOpopup_Scenarios,
                  fillColor = ~palScenario(Abs_death_Comb_S1),
                  fillOpacity = 0.8,
                  color = "white",
                  weight = 1,
                  group = "Scenario 1"
      )%>%
      addPolygons(data = CA_HO,
                  popup=SacHOpopup_Scenarios,
                  fillColor = ~palScenario(Abs_death_Comb_S2),
                  fillOpacity = 0.8,
                  color = "white",
                  weight = 1,
                  group = "Scenario 2"
      )%>%
      addPolygons(data = CA_HO,
                  popup=SacHOpopup_Scenarios,
                  fillColor = ~palScenario(Abs_death_Comb_S3),
                  fillOpacity = 0.8,
                  color = "white",
                  weight = 1,
                  group = "Scenario 3"
      )%>%
      
      #Add Layers Radio Buttons
      addLayersControl(
        baseGroups= c("Scenario 1", "Scenario 2", "Scenario 3"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE)
        )
      )
)

# Server Function ===========================================================================================================

server <- function(input, output, session){
  
# Set up leaflet basemap                       
  output$Basemap <- renderLeaflet({
    leaflet() %>%
      setView(-121, 39, 8) %>%
      addProviderTiles("CartoDB.Positron")
    
  })
  }




# App Function ===========================================================================================================
shinyApp(ui = ui, server = server)

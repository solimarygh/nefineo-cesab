library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(sf)

# Cargar tus datasets ya procesados (supone que ya están en el entorno)
# new.its1_sites_centroid, new.its2_sites_centroid, etc.

ui <- fluidPage(
  titlePanel("Mapa interactivo de centroides"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("datasets", "Selecciona datasets a mostrar:",
                         choices = c("ITS1", "ITS2", "ITSboth", 
                                     "atlasmxb", "diam", "dryf", 
                                     "long", "mymo", "spun"),
                         selected = c("ITS1", "ITS2", "ITSboth")),
      actionButton("reset", "Resetear selección")
    ),
    
    mainPanel(
      leafletOutput("mapa", height = 700)
    )
  )
)

server <- function(input, output, session) {
  
  # Mapa base
  output$mapa <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = Neotropic_Map ,  weight = 1, fillOpacity = 0.0)
  })
  
  # Resetear selección
  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "datasets", selected = character(0))
  })
  
  # Mostrar/ocultar puntos dinámicamente
  observe({
    leafletProxy("mapa") %>%
      clearGroup("ITS1") %>%
      clearGroup("ITS2") %>%
      clearGroup("ITSboth") %>%
      clearGroup("atlasmxb") %>%
      clearGroup("diam") %>%
      clearGroup("dryf") %>%
      clearGroup("long") %>%
      clearGroup("mymo") %>%
      clearGroup("spun")
    
    if ("ITS1" %in% input$datasets) {
      leafletProxy("mapa") %>%
        addCircles(data = new.its1_sites_centroid,
                   lng = ~centroid_long, lat = ~centroid_lat,
                   radius = ~n_aggregated_samples * 3,
                   color = "blue", group = "ITS1",
                   popup = ~paste0("<b>ID:</b> ", new.ID, "<br>",
                                   "<b>n_aggregated_samples:</b> ", n_aggregated_samples),
                   label = ~new.ID)
    }
    
    if ("ITS2" %in% input$datasets) {
      leafletProxy("mapa") %>%
        addCircles(data = new.its2_sites_centroid,
                   lng = ~centroid_long, lat = ~centroid_lat,
                   radius = ~n_aggregated_samples * 3,
                   color = "green", group = "ITS2",
                   popup = ~paste0("<b>ID:</b> ", new.ID, "<br>",
                                   "<b>n_aggregated_samples:</b> ", n_aggregated_samples),
                   label = ~new.ID)
    }
    
    if ("ITSboth" %in% input$datasets) {
      leafletProxy("mapa") %>%
        addCircles(data = new.itsboth_sites_centroid,
                   lng = ~centroid_long, lat = ~centroid_lat,
                   radius = ~n_aggregated_samples * 3,
                   color = "violet", group = "ITSboth",
                   popup = ~paste0("<b>ID:</b> ", new.ID, "<br>",
                                   "<b>n_aggregated_samples:</b> ", n_aggregated_samples),
                   label = ~new.ID)
    }
    
    if ("atlasmxb" %in% input$datasets) {
      leafletProxy("mapa") %>%
        addCircles(data = new_atlasmxb_centroid,
                   lng = ~centroid_long, lat = ~centroid_lat,
                   radius = 10, color = "red", group = "atlasmxb",
                   popup = ~paste0("<b>ID:</b> ", new.ID),
                   label = ~new.ID)
    }
    
    if ("diam" %in% input$datasets) {
      leafletProxy("mapa") %>%
        addCircles(data = new_diam_centroid,
                   lng = ~centroid_long, lat = ~centroid_lat,
                   radius = 10, color = "red", group = "diam",
                   popup = ~paste0("<b>ID:</b> ", new.ID),
                   label = ~new.ID)
    }
    
    if ("dryf" %in% input$datasets) {
      leafletProxy("mapa") %>%
        addCircles(data = new_dryf_centroid,
                   lng = ~centroid_long, lat = ~centroid_lat,
                   radius = 10, color = "red", group = "dryf",
                   popup = ~paste0("<b>ID:</b> ", new.ID),
                   label = ~new.ID)
    }
    
    if ("long" %in% input$datasets) {
      leafletProxy("mapa") %>%
        addCircles(data = new_long_centroid,
                   lng = ~centroid_long, lat = ~centroid_lat,
                   radius = 10, color = "red", group = "long",
                   popup = ~paste0("<b>ID:</b> ", new.ID),
                   label = ~new.ID)
    }
    
    if ("mymo" %in% input$datasets) {
      leafletProxy("mapa") %>%
        addCircles(data = new_mymo_centroid,
                   lng = ~centroid_long, lat = ~centroid_lat,
                   radius = 10, color = "red", group = "mymo",
                   popup = ~paste0("<b>ID:</b> ", new.ID),
                   label = ~new.ID)
    }
    
    if ("spun" %in% input$datasets) {
      leafletProxy("mapa") %>%
        addCircles(data = new_spun_centroid,
                   lng = ~centroid_long, lat = ~centroid_lat,
                   radius = 10, color = "red", group = "spun",
                   popup = ~paste0("<b>ID:</b> ", new.ID),
                   label = ~new.ID)
    }
  })
  
}

shinyApp(ui, server)

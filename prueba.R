# Instalar y cargar las librerías necesarias
library(shiny)
library(leaflet)
library(shinydashboard)

# Cargar los datos de calidad del aire en Valencia
# air_data <- read.csv("air_data_valencia.csv", header = TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "Valencia AQ"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      column(width = 4,
             box(title = "Visualización temporal",
                 width = NULL, solidHeader = TRUE,
                 actionButton("play", "Reproducir animación", icon = icon("play-circle")),
             ),
             box(title = "Variable a mostrar",
                 width = NULL, solidHeader = TRUE,
                 selectInput("var", NULL, choices = c("Índice de Calidad de Aire"))
             ),
             conditionalPanel(condition = "input.map_marker_click != null",
                              box(title = "Estadísticas de la estación seleccionada",
                                  width = NULL, solidHeader = TRUE,
                                  uiOutput("estadisticas_ui"),
                                  plotOutput("grafico", height = 300)))
      ),
      column(width = 8, leafletOutput("map", height = 900))
      
    ),
    
    
  )
)

# Crear el servidor
server <- function(input, output) {
  
  # Función para crear el mapa con Leaflet
  output$map <- renderLeaflet({
    
    # Crear el mapa con Leaflet
    map <- leaflet() %>% addTiles() %>%
      # addMarkers(~lon, ~lat, popup = ~paste0("Estación: ", estacion, "<br>",
      #                                        "NO2: ", no2, " µg/m3<br>",
      #                                        "PM10: ", pm10, " µg/m3<br>",
      #                                        "O3: ", o3, " µg/m3<br>",
      #                                        "SO2: ", so2, " µg/m3"))
      addMarkers(lat=39.4654570386165, lng=-0.3755875334642807, popup="The birthplace of R")
    
    # Retornar el mapa
    return(map)
    
  })
  
  # Función para mostrar las estadísticas de la estación seleccionada
  output$stats <- renderPrint({
    
    # Obtener la estación seleccionada
    selected_station <- input$map_marker_click
    
    # Si no hay ninguna estación seleccionada, retornar un mensaje vacío
    if (is.null(selected_station)) {
      return(NULL)
    }
    
    # Obtener las estadísticas de la estación seleccionada
    # station_stats <- air_data[air_data$estacion == selected_station$popup$estacion, ]
    
    # Formatear las estadísticas como un texto
    # station_stats_text <- paste("Estación:", station_stats$estacion, "<br>",
    #                             "NO2:", station_stats$no2, "µg/m3<br>",
    #                             "PM10:", station_stats$pm10, "µg/m3<br>",
    #                             "O3:", station_stats$o3, "µg/m3<br>",
    #                             "SO2:", station_stats$so2, "µg/m3")
    
    # Retornar las estadísticas como texto
    return(c("Hola"))
    
  })
  
}

shinyApp(ui, server)

# 1. Cargar todas las librerías necesarias
library(shiny)
library(leaflet)
library(IRISSeismic)
library(dplyr)
library(geojsonio)
library(shinydashboard)
library(DT)
library(shinycssloaders)
library(sp)
library(writexl) ## NUEVA LIBRERÍA: Para escribir archivos de Excel

# 2. Definición de la Interfaz de Usuario (UI)
ui <- dashboardPage(
  dashboardHeader(title = "|JD:MapaSísmico::Global|", titleWidth = 250),
  
  dashboardSidebar(
    width = 300,
    tags$style(type = "text/css", ".sidebar { height: 90vh; overflow-y: auto; }"),
    
    h4("Filtros de Datos", style = "padding-left : 15px;"),#padding-left text-align: center
    hr(),
    dateRangeInput(
      "fechas",
      "Rango de fechas:",
      start = Sys.Date() - 365,
      end = Sys.Date(),
      format = "yyyy-mm-dd",
      language = "es",
      width = "90%"
    ),
    
    sliderInput(
      "magnitud",
      "Rango de magnitud:",
      min = 3,
      max = 9,
      value = c(3, 9),
      step = 0.1,
      width = "90%"
    ),
    
    sliderInput(
      "profundidad",
      "Profundidad máxima (km):",
      min = 0,
      max = 700,
      value = 700,
      width = "90%"
    ),
    
    uiOutput("filtro_placa_ui"),
    
    radioButtons(
      "mapa_base",
      "Estilo de Mapa Base:",
      choices = c("Claro" = "CartoDB.Positron",
                  "Oscuro" = "CartoDB.DarkMatter",
                  "Satélite" = "Esri.WorldImagery"),
      selected = "CartoDB.Positron",
      width = "90%"
    ),
    
    checkboxInput(
      "mostrar_placas",
      "Mostrar placas tectónicas",
      value = TRUE,
      width = "90%"
    ),
    
    actionButton(
      "actualizar",
      "Actualizar Datos",
      icon = icon("sync"),
      width = "90%",
      class = "btn-primary"
    ),
    
    
    hr(),
    
    div(style = "text-align: center; padding: 10px;",
        h6("Fuente: Aplicación Shiny R Impulsada por IA/ con información de EarthScope Consortium",style = "margin-top: 5px; color:#87c3eb;"),
        
        tags$a(href="https://www.iris.edu/hq/", target="_blank",
               tags$img(src = "https://i.postimg.cc/kGWdcVyf/Captura-de-pantalla-2025-08-01-090858.png", height = "65px"))
    ),
    
    ## MODIFICACIÓN 1: Añadir un botón de descarga dedicado.
    
    br(), # Un pequeño espacio
    
    #hr(),
    downloadButton(
      "descargar_excel",
      "Descargar Base Completa (Excel)",
      class = "btn-success", # Botón verde para destacar
      style = "width: 90%; margin-left: 15px;"
    )
  ),
  
  dashboardBody(
    tabsetPanel(
      id = "tabs",
      tabPanel(
        "Mapa",
        value = "mapa_tab",
        tags$style(type = "text/css", "#mapa {height: calc(90vh - 80px) !important;}"), 
        withSpinner(leafletOutput("mapa", width = "100%", height = "100%"), color = "#3498db", type = 6)
      ),
      tabPanel(
        "Datos Sísmicos",
        value = "datos_tab",
        withSpinner(DTOutput("tabla_datos"), color = "#3498db", type = 6)
      )
    )
  )
)

# 3. Definición de la Lógica del Servidor (Server)
server <- function(input, output, session) {
  
  placas <- tryCatch({
    geojson_read("https://raw.githubusercontent.com/fraxen/tectonicplates/master/GeoJSON/PB2002_plates.json", what = "sp")
  }, error = function(e) {
    showNotification(paste("Error al cargar las placas tectónicas:", e$message), type = "error", duration = 10)
    return(NULL)
  })
  
  datos_procesados <- reactiveVal()
  
  output$filtro_placa_ui <- renderUI({
    req(placas)
    nombres_placas <- sort(unique(placas@data$PlateName))
    opciones <- setNames(nombres_placas, nombres_placas)
    opciones <- c("Todas las Placas" = "all", opciones)
    
    selectInput("filtro_placa", "Filtrar por Placa Tectónica:", choices = opciones, selected = "all", width = "90%")
  })
  
  observeEvent(input$actualizar, {
    req(input$filtro_placa)
    withProgress(message = 'Cargando datos sísmicos...', value = 0.5, {
      eventos_raw <- tryCatch({
        iris <- new("IrisClient")
        getEvent(iris, starttime = as.POSIXct(input$fechas[1], tz = "UTC"), endtime = as.POSIXct(input$fechas[2], tz = "UTC"), minmag = input$magnitud[1], maxmag = input$magnitud[2]) %>% as.data.frame()
      }, error = function(e){
        showNotification(paste("Error al obtener datos sísmicos:", e$message), type = "error", duration = 10)
        return(NULL)
      })
      
      if (!is.null(eventos_raw) && nrow(eventos_raw) > 0) {
        eventos_filtrados <- eventos_raw %>% filter(depth <= input$profundidad)
        sismos_sp <- SpatialPointsDataFrame(coords = eventos_filtrados[, c("longitude", "latitude")], data = eventos_filtrados, proj4string = CRS(proj4string(placas)))
        info_placas_sismos <- sp::over(sismos_sp, placas)
        eventos_con_placa <- sismos_sp@data %>% mutate(NombrePlaca = info_placas_sismos$PlateName)
        
        if ("eventLocationName" %in% names(eventos_con_placa)) {
          eventos_con_ubicacion <- eventos_con_placa %>% mutate(Ubicación = dplyr::if_else(is.na(eventLocationName) | eventLocationName == "", "No disponible", eventLocationName))
        } else {
          eventos_con_ubicacion <- eventos_con_placa %>% mutate(Ubicación = "No disponible")
        }
        
        if (input$filtro_placa != "all") {
          eventos_filtrados_placa <- eventos_con_ubicacion %>% filter(!is.na(NombrePlaca) & NombrePlaca == input$filtro_placa)
        } else {
          eventos_filtrados_placa <- eventos_con_ubicacion
        }
        
        eventos_finales <- eventos_filtrados_placa %>%
          arrange(desc(magnitude)) %>%
          mutate(
            Fecha = format(as.POSIXct(time, tz = "UTC"), "%Y-%m-%d %H:%M:%S"),
            Color = case_when(magnitude >= 7 ~ "#e41a1c", magnitude >= 6 ~ "#ff7f00", magnitude >= 5 ~ "#4daf4a", TRUE ~ "#377eb8")
          ) %>%
          select(Fecha, Ubicación, Magnitud = magnitude, Profundidad = depth, Latitud = latitude, Longitud = longitude, Color, NombrePlaca)
        
        datos_procesados(eventos_finales)
        
      } else if (!is.null(eventos_raw) && nrow(eventos_raw) == 0) {
        showNotification("No se encontraron sismos para los filtros seleccionados.", type = "warning", duration = 5)
        datos_procesados(NULL)
      }
    })
  }, ignoreNULL = FALSE)
  
  output$mapa <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(isolate(input$mapa_base), group = "base_map") %>%
      setView(lng = -30, lat = 20, zoom = 2) %>%
      addLegend(position = "bottomright", colors = c("#377eb8", "#4daf4a", "#ff7f00", "#e41a1c"), labels = c("3.0 - 4.9", "5.0 - 5.9", "6.0 - 6.9", "≥ 7.0"), title = "Magnitud del Sismo", opacity = 0.8)
  })
  
  observeEvent(input$mapa_base, {
    leafletProxy("mapa") %>% clearGroup("base_map") %>% addProviderTiles(input$mapa_base, group = "base_map")
  })
  
  observe({
    datos <- datos_procesados()
    proxy <- leafletProxy("mapa")
    proxy %>% clearGroup("Sismos")
    
    if(!is.null(datos) && nrow(datos) > 0){
      proxy %>% addCircleMarkers(
        data = datos, lng = ~Longitud, lat = ~Latitud, radius = ~Magnitud * 1.8, color = ~Color, fillColor = ~Color, fillOpacity = 0.7, stroke = TRUE, weight = 1,
        popup = ~paste("<div style='font-size:14px; width:250px;'>", "<h4 style='margin-top:0; color:#2c3e50;'>Información del Sismo</h4>", "<b>Ubicación:</b> ", Ubicación, "<br>", "<b>Fecha:</b> ", Fecha, "<br>", "<b>Magnitud:</b> ", Magnitud, "<br>", "<b>Profundidad:</b> ", Profundidad, " km<br>", "<b>Coordenadas:</b> ", round(Latitud, 4), "°N, ", round(Longitud, 4), "°E", "<br>", "<b>Placa Tectónica:</b>", ifelse(is.na(NombrePlaca), "N/A", NombrePlaca), "</div>"),
        label = ~paste("Magnitud:", Magnitud), group = "Sismos"
      )
    }
  })
  
  observe({
    req(input$filtro_placa, placas)
    proxy <- leafletProxy("mapa")
    proxy %>% clearGroup("Placas Tectónicas") %>% clearGroup("Placa Resaltada")
    
    if (input$mostrar_placas) {
      proxy %>% addPolylines(data = placas, color = "#808080", weight = 1.5, opacity = 0.7, group = "Placas Tectónicas", label = ~PlateName)
      if(input$filtro_placa != "all"){
        placa_seleccionada <- placas[placas@data$PlateName == input$filtro_placa, ]
        if(nrow(placa_seleccionada) > 0) {
          proxy %>% addPolylines(data = placa_seleccionada, color = "#db3445", weight = 3, opacity = 1, group = "Placa Resaltada", label = ~PlateName)
        }
      }
    }
  })
  
  output$tabla_datos <- renderDT({
    datos <- datos_procesados()
    if (is.null(datos)) {
      datos <- data.frame(Fecha=character(), Ubicación=character(), Magnitud=numeric(), Profundidad=numeric(), `Placa Tectónica`=character(), check.names = FALSE)
    }
    datatable(
      datos %>% select(-Color, -Latitud, -Longitud) %>% rename(`Placa Tectónica` = NombrePlaca) %>% arrange(desc(Magnitud)),
      extensions = 'Buttons',
      options = list(pageLength = 10, dom = 'Bfrtip', buttons = list(list(extend = 'copy', text = 'Copiar'), list(extend = 'csv', text = 'CSV', filename = "datos_sismicos", charset = 'utf-8', bom = TRUE), list(extend = 'excel', text = 'Excel', filename = "datos_sismicos", charset = 'utf-8'), list(extend = 'pdf', text = 'PDF', filename = "datos_sismicos", orientation = 'landscape')), scrollX = TRUE, language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'), order = list()),
      rownames = FALSE, filter = 'top'
    ) %>% formatRound(columns = c('Magnitud', 'Profundidad'), digits = 2)
  })
  
  ## MODIFICACIÓN 2: Añadir la lógica de descarga (downloadHandler).
  output$descargar_excel <- downloadHandler(
    filename = function() {
      paste0("datos_sismicos_completos_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # Acceder a los datos completos desde el reactiveVal
      datos_a_descargar <- datos_procesados()
      
      # Comprobar si hay datos para descargar
      if (is.null(datos_a_descargar) || nrow(datos_a_descargar) == 0) {
        # Crear un dataframe vacío con nombres de columna para no generar un error
        datos_a_descargar <- data.frame(
          Fecha = character(),
          Ubicación = character(),
          Magnitud = numeric(),
          Profundidad_km = numeric(),
          Latitud = numeric(),
          Longitud = numeric(),
          `Placa Tectónica` = character(),
          check.names = FALSE
        )
        showNotification("No hay datos para descargar con los filtros actuales.", type = "warning", duration = 5)
      } else {
        # Preparar el dataframe para la descarga: seleccionar y renombrar columnas
        datos_a_descargar <- datos_a_descargar %>%
          select(Fecha, Ubicación, Magnitud, Profundidad, Latitud, Longitud, NombrePlaca) %>%
          rename(
            Profundidad_km = Profundidad,
            `Placa Tectónica` = NombrePlaca
          )
      }
      
      # Escribir el archivo Excel
      writexl::write_xlsx(datos_a_descargar, path = file)
    }
  )
}

# 4. Ejecutar la aplicación
shinyApp(ui, server)
